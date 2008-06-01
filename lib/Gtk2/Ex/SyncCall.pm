# Copyright 2008 Kevin Ryde

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

package Gtk2::Ex::SyncCall;
use strict;
use warnings;
use Carp;
use Gtk2;
use Glib::Ex::SignalIds;

our $VERSION = 2;

# set this to 1 for some diagnostic prints
use constant DEBUG => 0;


my $sync_call_atom;

sub sync {
  my ($class, $widget, $callback, $userdata) = @_;
  if (DEBUG) { print "SyncCall sync\n"; }

  my $display = $widget->get_display;
  my $data = ($display->{'Gtk2::Ex::SyncCall'} ||= do {
    $widget->add_events ('property-change-mask');
    ({ sync_list => [],
       signal_ids => Glib::Ex::SignalIds->new
       ($widget,
        $widget->signal_connect (property_notify_event
                                 => \&_do_property_notify),
        $widget->signal_connect (unrealize => \&_do_widget_destroy),
        $widget->signal_connect (destroy   => \&_do_widget_destroy)) })
  });

  $widget = $data->{'signal_ids'}->object;
  my $win = $widget->window
    or croak "Gtk2::Ex::SyncCall->sync(): widget not realized";

  my $self = { display  => $display,
               callback => $callback,
               userdata => $userdata };
  my $aref = $data->{'sync_list'};
  push @$aref, $self;

  if (@$aref == 1) {
    $sync_call_atom ||= Gtk2::Gdk::Atom->intern (__PACKAGE__);
    if (DEBUG) { print "  change $sync_call_atom\n"; }
    $win->property_change ($sync_call_atom,
                           Gtk2::Gdk::Atom->intern('STRING'),
                           Gtk2::Gdk::CHARS, 'append', '');
  }
  return $self;
}

sub _do_property_notify {
  my ($widget, $event) = @_;

  # note, no overloaded != in Gtk2-Perl 1.181, only ==
  if ($event->atom == $sync_call_atom) {
    if (DEBUG) { print "SyncCall notify $widget, ",$event->atom,"\n"; }

    my $display = $widget->get_display;
    my $data = $display->{'Gtk2::Ex::SyncCall'};
    _call_all ($data);
  }
  # even though $sync_call_atom is supposed to be for us alone, propagate it
  # anyway in case someone else is monitoring what happens
  return 0;  # propagate event
}

sub _do_widget_destroy {
  my ($widget) = @_;
  my $display = $widget->get_display;
  if (my $data = delete $display->{'Gtk2::Ex::SyncCall'}) {
    _call_all ($data);
  }
}

sub _call_all {
  my ($data) = @_;
  my $aref = $data->{'sync_list'};
  while (my $self = pop @$aref) {
    $self->{'callback'}->($self->{'userdata'});
  }
}

1;
__END__

=head1 NAME

Gtk2::Ex::SyncCall -- server sync callback

=head1 SYNOPSIS

 use Gtk2::Ex::SyncCall;
 Gtk2::Ex::SyncCall->sync ($widget, sub { some code; });

=head1 DESCRIPTION

C<Gtk2::Ex::SyncCall> sends a dummy synchronizing request to the X server
and calls back to your code when response arrives back.  This is like
C<< Gtk2::Gdk::Display::sync() >>, but as a callback instead of blocking.

A sync like this is a good way to avoid hammering the server with more
drawing requests etc than it can keep up with.  Choosing where in your
program is a good place to pause is of course up to you.

=head2 Implmentation

SyncCall is done with a dummy property change on the given C<$widget>, so
the widget must be realized.  The property change setups on that widget are
left installed ready for further use to the same display.  An unrealize or
destroy on the widget will call pending callbacks and then reset ready for a
different widget on subsequent syncs.

It's a good idea if C<$widget> isn't your top-level C<Gtk2::Window> widget,
since generally the window manager listens for property changes on that.
The C<"Gtk2::Ex::SyncCall"> property will be ignored by it, but it's a
little wasteful to have it see unnecessary change events.

(There's various alternatives to this approach.  Something not directly
involving a widget would be better, the widget then only indicating the
target display.)

=head1 FUNCTIONS

=over 4

=item C<< Gtk2::Ex::SyncCall->sync ($widget, $coderef, $userdata) >>

Send a dummy synchronizing message to the X server and when it replies call
C<< $coderef->($userdata) >>.  C<$widget> must be realized
(C<< $widget->realize() >>).

Multiple C<sync> calls on the same display are collected up so just one
synchronising message is sent, with all the registered callbacks then done
when the reply comes.

=back

=head1 SEE ALSO

L<Gtk2::Widget>, L<Gtk2::Gdk::Display>

=cut
