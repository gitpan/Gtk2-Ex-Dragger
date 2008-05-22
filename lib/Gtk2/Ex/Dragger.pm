# Copyright 2007, 2008 Kevin Ryde

# This file is part of Gtk2-Ex-Dragger.
#
# Gtk2-Ex-Dragger is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any later
# version.
#
# Gtk2-Ex-Dragger is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License along
# with Gtk2-Ex-Dragger.  If not, see <http://www.gnu.org/licenses/>.

package Gtk2::Ex::Dragger;
use strict;
use warnings;
use Carp;
use POSIX qw(floor ceil);
use Gtk2;
use List::Util qw(min max);
use Scalar::Util;

our $VERSION = 1;

# set this to 1 or 2 for some diagnostic prints
use constant DEBUG => 0;


use constant {
  # not wrapped in Gtk2-Perl 1.181
  GDK_CURRENT_TIME => 0,
  GDK_PRIORITY_REDRAW => Glib::G_PRIORITY_HIGH_IDLE + 20,

  DELAY_MILLISECONDS => 3,
};

sub new {
  my ($class, %self) = @_;
  my $self = bless \%self, $class;

  my $widget = $self->{'widget'};
  (Scalar::Util::blessed($widget) && $widget->isa('Gtk2::Widget'))
    or croak "Dragger needs 'widget' parameter to be a Gtk2::Widget";
  Scalar::Util::weaken ($self->{'widget'});

  my $hadj = $self->{'hadjustment'};
  my $vadj = $self->{'vadjustment'};
  ($hadj || $vadj)
    or carp "Dragger has neither hadjustment nor vadjustment, nothing can move\n";

  # require Gtk2::Ex::WidgetEvents;
  # $self->{'wevents'} = Gtk2::Ex::WidgetEvents->new ($widget,
  #                                                   ['button-press-mask',
  #                                                    'button-motion-mask',
  #                                                    'button-release-mask']);
  $widget->add_events (['button-press-mask',
                        'button-motion-mask',
                        'button-release-mask']);

  my $ref_weak_self = _ref_weak ($self);
  require Glib::Ex::SignalIds;
  $self->{'wsigs'} = Glib::Ex::SignalIds->new
    ($widget,
     $widget->signal_connect (motion_notify_event => \&_do_motion_notify,
                              $ref_weak_self),
     $widget->signal_connect (button_release_event => \&_do_button_release,
                              $ref_weak_self),
     $widget->signal_connect (configure_event => \&_do_configure_event,
                              $ref_weak_self),
     $widget->signal_connect (grab_broken_event => \&_do_grab_broken,
                              $ref_weak_self));

  if ($hadj) {
    $self->{'hadjustment_ids'} = Glib::Ex::SignalIds->new
      ($hadj,
       $hadj->signal_connect (changed => \&_do_adjustment_changed,
                              $ref_weak_self));
  }
  if ($vadj) {
    $self->{'vadjustment_ids'} = Glib::Ex::SignalIds->new
      ($vadj,
       $vadj->signal_connect (changed => \&_do_adjustment_changed,
                              $ref_weak_self));
  }
  return $self;
}

sub DESTROY {
  my ($self) = @_;
  $self->stop;
}

sub start {
  my ($self, $event) = @_;

  (Scalar::Util::blessed($event) && $event->isa('Gtk2::Gdk::Event::Button'))
    or croak "Dragger->start(): need button event";

  my $widget = $self->{'widget'};
  my $win = $widget->Gtk2_Ex_Dragger_window
    or croak "Dragger->start(): widget not realized";

  if (exists $self->{'cursor'}) {
    require Gtk2::Ex::WidgetCursor;
    $self->{'wcursor'} = Gtk2::Ex::WidgetCursor->new
      (widget => $widget,
       cursor => $self->{'cursor'},
       active => 1);
  }

  $self->{'button'} = $event->button;
  $self->{'active'} = 1;

  my $weak_self = _ref_weak ($self);

  if ($self->{'confine'}) {
    my $confine_win = ($self->{'confine_win'} ||= Gtk2::Gdk::Window->new
                       ($widget->get_root_window,
                        { window_type => 'temp',
                          wclass => 'GDK_INPUT_ONLY',
                          override_redirect => 1 }));

    _establish_drag_base ($self, $event->x, $event->y);
    $confine_win->show;

    my $event_mask = [ 'button-motion-mask', 'button-release-mask' ]
      + ($win->get_events & ['button-press-mask',
                             'pointer-motion-hint-mask',
                             'structure-mask',
                             'property-change-mask' ]);
    if (DEBUG) { print "  events $event_mask\n"; }

    my $status = Gtk2::Gdk->pointer_grab ($win,
                                          0,      # owner events
                                          $event_mask,
                                          $confine_win,
                                          undef,  # cursor inherited
                                          $event->time);
    if ($status eq 'success') {
      $self->{'grabbed'} = 1;
    } else {
      warn "Dragger: cannot grab: $status";
    }
  } else {
    _establish_drag_base ($self, $event->x, $event->y);
  }
}

sub _do_grab_broken {
  my ($widget, $event, $ref_weak_self) = @_;
  if (DEBUG) { print "Dragger grab broken\n"; }
  my $self = $$ref_weak_self or return;
  $self->{'grabbed'} = 0;
  $self->stop ($event);
  return 0; # propagate event
}

sub stop {
  my ($self, $event) = @_;
  if (DEBUG) { print "Dragger stop\n"; }

  if (! $self->{'active'}) { return; }
  $self->{'active'} = 0;

  if ($self->{'grabbed'}) {
    $self->{'grabbed'} = 0;
    Gtk2::Gdk->pointer_ungrab (defined $event
                               ? $event->time : GDK_CURRENT_TIME);
  }
  if ($self->{'confine_win'}) {
    $self->{'confine_win'}->hide;
  }
  delete $self->{'wcursor'};
  _emit_pending ($self);
}

sub _establish_drag_base {
  my ($self, $x, $y) = @_;
  my $widget = $self->{'widget'};
  my $win = $widget->Gtk2_Ex_Dragger_window;
  my ($win_width, $win_height) = $win->get_size;

  my $root = $widget->get_root_window;
  my ($root_width, $root_height) = $root->get_size;
  my ($win_root_x, $win_root_y) = $win->get_origin;

  my ($confine_x, $confine_y, $confine_width, $confine_height);

  if (my $hadj = $self->{'hadjustment'}) {
    $self->{'x_base'} = $hadj->value
      + ($self->{'hinverted'} ? -$x : $x) * ($hadj->page_size / $win_width);
    if (DEBUG) { print "  base x=$x v=", $hadj->value,
                   " set base=", $self->{'x_base'}, "\n"; }

    if ($self->{'hinverted'}) {
      $confine_x = $win_root_x + $x - $win_width
        * ($hadj->value - $hadj->lower) / $hadj->page_size;
    } else {
      $confine_x = $win_root_x + $x - $win_width
        * ($hadj->upper - $hadj->page_size - $hadj->value) / $hadj->page_size;
    }
    $confine_width = $win_width
      * ($hadj->upper - $hadj->lower - $hadj->page_size) / $hadj->page_size;

  } else {
    $confine_x = 0;
    $confine_width = $root_width;
  }

  if (my $vadj = $self->{'vadjustment'}) {
    $self->{'y_base'} = $vadj->value
      + ($self->{'vinverted'} ? -$y : $y) * ($vadj->page_size / $win_height);

    if ($self->{'vinverted'}) {
      $confine_y = $win_root_y + $y - $win_height
        * ($vadj->value - $vadj->lower) / $vadj->page_size;
    } else {
      $confine_y = $win_root_y + $y - $win_height
        * ($vadj->upper - $vadj->page_size - $vadj->value) / $vadj->page_size;
    }
    $confine_height = $win_height
      * ($vadj->upper - $vadj->lower - $vadj->page_size) / $vadj->page_size;

  } else {
    $confine_y = 0;
    $confine_height = $root_height;
  }

  if (! $self->{'confine'}) { return; }

  my $frac;
  ($confine_x, $frac) = floor_and_frac ($confine_x);
  $confine_width += $frac;
  $confine_x--;

  ($confine_y, $frac) = floor_and_frac ($confine_y);
  $confine_height += $frac;
  $confine_y--;

  $confine_width  = ceil ($confine_width) + 1;
  $confine_height = ceil ($confine_height) + 1;

  if ($confine_x < 0) {
    $confine_width += $confine_x;  # reduce width accordingly
    $confine_x = 0;
  }
  if ($confine_y < 0) {
    $confine_height += $confine_y;  # reduce height accordingly
    $confine_y = 0;
  }

  if ($confine_x >= $root_width) {
    $confine_x = $root_width - 1;
    $confine_width = 1;
  }
  if ($confine_y >= $root_height) {
    $confine_y = $root_height - 1;
    $confine_height = 1;
  }

  $confine_width  = min ($confine_width,  $root_width  - $confine_x);
  $confine_height = min ($confine_height, $root_height - $confine_y);
  $confine_width  = max ($confine_width,  1);
  $confine_height = max ($confine_height, 1);

  if (DEBUG) { print "confine $confine_x,$confine_y",
                 "  ${confine_width}x${confine_height}\n"; }
  $self->{'confine_win'}->move_resize ($confine_x, $confine_y,
                                       $confine_width, $confine_height);
}

# return two values ($floor, $frac) which are $x rounded down to an integer,
# and the fractional part subtracted from $x to get to that integer
sub floor_and_frac {
  my ($x) = @_;
  my $f = floor ($x);
  return ($f, $x - $f);
}

# $widget->Gtk2_Ex_Dragger_window() returns the window in $widget that the
# dragger should operate on
#
sub Gtk2::Widget::Gtk2_Ex_Dragger_window {
  my ($widget) = @_;
  if (exists $widget->{'Gtk2_Ex_Dragger_window'}) {
    return $widget->{'Gtk2_Ex_Dragger_window'};  # user override
  } else {
    return $widget->window;  # default
  }
}
sub Gtk2::TextView::Gtk2_Ex_Dragger_window {
  my ($textview) = @_;
  return $textview->get_window ('text');
}
*Gtk2::TreeView::Gtk2_Ex_Dragger_window = \&get_bin_window;

sub _do_motion_notify {
  my ($widget, $event, $ref_weak_self) = @_;
  my $self = $$ref_weak_self or return;
  if (! $self->{'active'}) { return 0; } # propagate event
  if (DEBUG >= 2) { print "Dragger motion\n"; }

  my ($x, $y);
  if ($widget->get_events & 'pointer-motion-hint-mask') {
    ($x, $y) = $widget->get_pointer;
  } else {
    $x = $event->x;
    $y = $event->y;
  }

  my $win = $widget->Gtk2_Ex_Dragger_window;
  my ($win_width, $win_height) = $win->get_size;

  if (my $hadj = $self->{'hadjustment'}) {
    my $x_base = $self->{'x_base'};
    my $page_size = $hadj->page_size;
    my $value = $x_base
      - ($self->{'hinverted'} ? -$x : $x) * ($page_size / $win_width);
    _set_value ($self, 'hadjustment', $value);
  }
  if (my $vadj = $self->{'vadjustment'}) {
    my $y_base = $self->{'y_base'};
    my $page_size = $vadj->page_size;
    my $value = $y_base
      - ($self->{'vinverted'} ? -$y : $y) * ($page_size / $win_height);
    _set_value ($self, 'vadjustment', $value);
  }
  return 0; # propagate event
}

sub _do_button_release {
  my ($widget, $event, $ref_weak_self) = @_;
  if (DEBUG) { print "Dragger button release\n"; }
  my $self = $$ref_weak_self or return;
  if (! $self->{'active'} || $event->button != $self->{'button'}) {
    return 0;  # propagate event
  }
  _do_motion_notify ($widget, $event, \$self);
  $self->stop ($event);
  return 0; # propagate event
}

sub _do_configure_event {
  my ($widget, $event, $ref_weak_self) = @_;
  my $self = $$ref_weak_self or return;
  if (! $self->{'active'}) { return 0; }  # propagate event

  # if the window size has changed then the scale is different, making a
  # different base point, so recalculate
  my ($x, $y) = $self->{'widget'}->get_pointer;
  _establish_drag_base ($self, $x, $y);
  return 0;  # propagate event
}

sub _do_adjustment_changed {
  my ($adj, $ref_weak_self) = @_;
  my $self = $$ref_weak_self or return;
  if (! $self->{'active'}) { return; }

  # if the page size has changed then the scale is different, making a
  # different base point, so recalculate
  my ($x, $y) = $self->{'widget'}->get_pointer;
  _establish_drag_base ($self, $x, $y);
}

sub _set_value {
  my ($self, $which, $value) = @_;
  my $adj = $self->{$which};

  # gtk_adjustment_set_value() clamps to lower<=v<=upper, but want to take
  # into account page_size too; and must clamp ourselves if using the field
  # setter
  $value = min ($value, $adj->upper - $adj->page_size);
  $value = max ($value, $adj->lower);

  my $update_policy = $self->{'update_policy'} || 'default';

  if (DEBUG >= 2) { print "  $which $value  within ",
                      $adj->lower," to ",$adj->upper,
                        " page ",$adj->page_size,
                          "  with $update_policy\n"; }

  if ($update_policy eq 'discontinuous') {
    return;
  }
  if ($update_policy eq 'continuous') {
    $adj->set_value ($value);
    return;
  }

  $adj->value ($value);
  $self->{'pending',$which} = 1;

  if ($update_policy eq 'delayed') {
    $self->{'timer_id'} ||= Glib::Timeout->add
      (DELAY_MILLISECONDS, \&_do_timer_delayed, _ref_weak ($self));

  } else {
    $self->{'sync_obj'} ||= do {
      if (DEBUG >= 2) { print "Dragger sync send\n"; }
      $self->{'timer_id'} ||= Glib::Timeout->add
        (DELAY_MILLISECONDS, \&_do_timer_sync, _ref_weak ($self));

      require Gtk2::Ex::SyncCall;
      Gtk2::Ex::SyncCall->sync ($self->{'widget'}, \&_do_sync,
                                _ref_weak ($self));
    };
  }
}

# timer expiry for 'delayed' policy
# 'value-changed' is emitted when the timer expires
#
sub _do_timer_delayed {
  my ($ref_weak_self) = @_;
  if (DEBUG >= 2) { print "Dragger timer for 'delayed'\n"; }
  my $self = $$ref_weak_self or return 0; # remove timer

  delete $self->{'timer_id'};
  _emit_pending ($self);
  return 0; # remove timer
}

# sync response for 'default' policy
#
# At this point we wait for the timer or for idle, whichever comes first.
# It's possible the timer has already gone off (deleting 'timer_id'), if
# that's the case them we emit immediately; otherwise we start an idle.
#
sub _do_sync {
  my ($ref_weak_self) = @_;
  if (DEBUG >= 2) { print "Dragger sync response\n"; }
  my $self = $$ref_weak_self or return;

  delete $self->{'sync_obj'};
  if ($self->{'timer_id'}) {
    $self->{'idle_id'} ||= Glib::Idle->add
      (\&_do_idle, _ref_weak ($self), GDK_PRIORITY_REDRAW - 1);
  } else {
    _emit_pending ($self);
  }
}

# timer expiry for 'default' policy

# If the sync response hasn't yet been received then we do nothing, instead
# wait for that.  If it has been received then we can emit now, and cancel
# the idle that was running.
#
sub _do_timer_sync {
  my ($ref_weak_self) = @_;
  my $self = $$ref_weak_self or return 0; # remove timer
  if (DEBUG >= 2) { print "Dragger timer for sync; with sync ",
                      $self->{'sync_obj'} ? "still running\n" : "finished\n"; }

  if (my $id = delete $self->{'idle_id'}) {
    Glib::Source->remove ($id);
  }

  delete $self->{'timer_id'};
  if (! $self->{'sync_obj'}) {
    _emit_pending ($self);
  }
  return 0; # remove timer
}

# idle handler for 'default' policy
sub _do_idle {
  my ($ref_weak_self) = @_;
  if (DEBUG >= 2) { print "Dragger idle after sync\n"; }
  my $self = $$ref_weak_self or return 0; # remove idle

  if (my $id = delete $self->{'timer_id'}) {
    Glib::Source->remove ($id);
  }
  delete $self->{'idle_id'};
  _emit_pending ($self);
  return 0; # remove idle
}

sub _emit_pending {
  my ($self) = @_;
  my $sync = 0;
  foreach my $which ('hadjustment', 'vadjustment') {
    if (delete $self->{'pending',$which}) {
      $self->{$which}->value_changed;
      $sync ||= do { $self->{'widget'}->get_display->sync; 1 };
    }
  }
}

sub _ref_weak {
  my ($self) = @_;
  Scalar::Util::weaken ($self);
  return \$self;
}

1;
__END__

=head1 NAME

Gtk2::Ex::Dragger -- drag to move adjustment position

=head1 SYNOPSIS

 use Gtk2::Ex::Dragger;
 Gtk2::Ex::Dragger->new (widget => $widget,
                         hadjustment => $widget->get_hadjustment);

=head1 DESCRIPTION

C<Gtk2::Ex::Dragger> implements mouse pointer dragging to move the contents
of a widget horizontally, vertically, or both.  It works on any windowed
widget with C<Gtk2::Adjustment> objects controlling the visible area.

The width or height of the widget corresponds to the "page" in the
adjustment and Dragger scales pixel movement to the adjustment "value"
accordingly.  It's then up to the usual widget drawing code to follow
C<value-changed> signals from the adjustment for redraws, the same as for
scrollbars etc.  The effect for the user is that the contents get pulled
around with the mouse.

                 Adjustment
                      +--+ --upper
                      |  |
     Window           |  |
    +-------------+ \ |  |
    |             |  \|  |
    |             |   +--+ \_ page size
    |             |   |  | /
    |             |   +--+ ___ value
    |             |  /|  |
    +-------------+ / |  |
                      |  |
                      |  |
                      +--+ --lower

If you've got scrollbars attached to the adjustments then they move with the
dragging too.  It can be good to have scrollbars together with draggability
since the scrollbars give visual feedback but the dragging allows fine
movements if the visible page is only a small part of the adjustment
extents.

The "confine" option allows you to restict mouse movement to screen
positions corresponding to the adjustement extents, so the user gets an
obvious feedback at the limits.

The "cursor" option changes the mouse pointer cursor while dragging.  This
is good if it's not obvious in a given widget which button press etc
activates a drag.  The cursor is set through C<Gtk2::Ex::WidgetCursor> and
so cooperates with other uses of that (such as the global "busy"
indication).

Dragger can work on both natively scrollable widgets and widgets put into a
C<Gtk2::Viewport>.  For a viewport it's the viewport widget which should be
passed to the dragger (it being the "visible" part of a larger underlying
thing).

Changes to the adjustment extents or page size or to the widget window size
are allowed during a drag, though currently any external changes to the
"value", ie. the position, during a drag will be overridden.

=head1 FUNCTIONS

=over 4

=item C<< Gtk2::Ex::Dragger->new (key=>value, ...) >>

Create and return a new dragger.  Key/value pairs set the following various
parameters,

    widget         the widget to drag
    hadjustment    Gtk2::Adjustment
    vadjustment    Gtk2::Adjustment
    hinverted      boolean
    vinverted      boolean
    cursor         cursor name per Gtk2::Ex::WidgetCursor
    confine        boolean
    update_policy  string

The target C<widget> and at least one of C<hadjustment> or C<vadjustment>
are mandatory, the rest are options.

The C<hinverted> or C<vinverted> flags swap the direction the adjustments
are moved.  Normally the C<hadjustment> value increases to the left and the
C<vadjustment> downwards.  Inverting goes instead to the right, and upwards.
This is the same sense as C<Gtk2::Scrollbar> uses, so if you've set
C<inverted> on your scrollbar then do the same to the dragger.

C<cursor> is any cursor name or object accepted by the WidgetCursor
mechanism (see L<Gtk2::Ex::WidgetCursor>).  If unset or undef then the
cursor is unchanged (and you don't need to have WidgetCursor installed in
that case).

C<update_policy> is a string option for how often to emit C<value-changed>
signals on the adjustments.  C<"continous"> means every motion event,
C<"delayed"> means after 300 milliseconds (to collapse multiple motions), or
C<"discontinuous"> means only on ending the drag.  But the secret default
you get without an C<update_policy> option is designed to be a good
compromise between smoothness and collapsing.  It syncs with the X server to
avoid hammering, folowed by a wait for idle or timeout on the client side,
whichever comes first.

=item C<< $dragger->start ($event) >>

Begin a drag.  C<$event> must be a C<Gtk2::Gdk::Event::Button> button press
object; it gives the button doing the drag (and a server timestamp).

=item C<< $dragger->stop () >>

=item C<< $dragger->stop ($event) >>

Stop C<$dragger>, if it's active.  Normally a dragger stops itself when the
dragging button is released, but this method can be do it sooner.

If you stop in response to a Gdk event then pass the C<Gtk2::Gdk::Event> so
its timestamp can be used.  (This matters with the confine option if event
processing is lagged; ensuring any later button passive grab isn't undone.)

=back

=head1 OTHER NOTES

The C<update_policy> option is the same sort of thing C<Gtk2::Scrollbar>
has, and is done in the same way, namely the C<value> in the adjustment is
set, but the C<value-changed> signal emission may be deferred for a while.
Choosing a policy is really a matter of how good the drawing your target
widget is.  You can see the difference for example with a big block of text
in a C<Gtk2::TextView> versus a viewport and C<Gtk2::Label>.  The TextView
goes very close to coping with C<continuous> update policy, but the same on
the Label's naive drawing ends up flooding the server to the point of being
unusable.

Dragger recognises C<pointer-motion-hint-mask> flag in the events mask of
its target widget and will do a C<< $widget->get_pointer >> in each motion
event processed.  This means a deliberate server round-trip on each move,
which has the effect of making each motion wait until the drawing etc from
the previous one has finished.  Give this a try if you're having trouble
with excessive redrawing going to the server, though the default update
policy is meant to achieve the same effect asynchronously.

Currently only a weak reference is kept to the target widget, so the fact
there's a dragger feature created doesn't keep it alive forever.  This means
in particular it's safe to hold the dragger object in the widget instance
data without creating a circular reference.  But strong references are kept
to the adjustment objects since they probably should stay alive as long as
the widget and dragger do.  But perhaps this will be changed.

=head1 SEE ALSO

L<Gtk2::Adjustment>, L<Gtk2::Ex::WidgetCursor>

=head1 HOME PAGE

L<http://www.geocities.com/user42_kevin/gtk2-ex-dragger/index.html>

=head1 LICENSE

Copyright 2007, 2008 Kevin Ryde

Gtk2-Ex-Dragger is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

Gtk2-Ex-Dragger is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
more details.

You should have received a copy of the GNU General Public License along with
Gtk2-Ex-Dragger.  If not, see L<http://www.gnu.org/licenses/>.

=cut
