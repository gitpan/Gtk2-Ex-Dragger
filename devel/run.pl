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


use strict;
use warnings;
use List::Util qw(min max);
use Gtk2 '-init';
use Gtk2::Ex::Dragger;
use Data::Dumper;

# Gtk2::Gdk::Window->set_debug_updates (1);

Gtk2::Rc->parse_string (<<'HERE');
style "My_style"
  {
    fg[ACTIVE]        = { 1.0, 1.0, 1.0 }
    fg[NORMAL]        = { 1.0, 1.0, 1.0 }
    fg[PRELIGHT]      = { 1.0, 1.0, 1.0 }
    fg[SELECTED]      = { 1.0, 1.0, 1.0 }
    fg[INSENSITIVE]   = { 1.0, 1.0, 1.0 }
    text[ACTIVE]      = { 1.0, 1.0, 1.0 }
    text[NORMAL]      = { 1.0, 1.0, 1.0 }
    text[PRELIGHT]    = { 1.0, 1.0, 1.0 }
    text[SELECTED]    = { 1.0, 1.0, 1.0 }
    text[INSENSITIVE] = { 1.0, 1.0, 1.0 }
    bg[ACTIVE]        = { 0, 0, 0 }
    bg[NORMAL]        = { 0, 0, 0 }
    bg[PRELIGHT]      = { 0, 0, 0 }
    bg[SELECTED]      = { 0, 0, 0 }
    bg[INSENSITIVE]   = { 0, 0, 0 }
    base[ACTIVE]      = { 0, 0, 0 }
    base[NORMAL]      = { 0, 0, 0 }
    base[PRELIGHT]    = { 0, 0, 0 }
    base[SELECTED]    = { 0, 0, 0 }
    base[INSENSITIVE] = { 0, 0, 0 }
  }
widget "*.GtkDrawingArea" style "My_style"
HERE

my $toplevel = Gtk2::Window->new('toplevel');
$toplevel->signal_connect (destroy => sub { Gtk2->main_quit });

my $hbox = Gtk2::HBox->new;
$toplevel->add ($hbox);

my $vbox = Gtk2::VBox->new;
$hbox->pack_start ($vbox, 0,0,0);

my $table = Gtk2::Table->new (2, 2, 0);
$hbox->pack_start ($table, 0,0,0);

my $area = Gtk2::DrawingArea->new;
$area->set_size_request (200, 200);
$table->attach ($area, 0,1, 0,1,
                ['expand','shrink','fill'],['expand','shrink','fill'], 0,0);
$area->set_flags ('can-focus');
$area->grab_focus;

my $vadj = Gtk2::Adjustment->new (100, 0, 300, 1, 10, 100);
my $vscroll = Gtk2::VScrollBar->new ($vadj);
$table->attach ($vscroll, 1,2, 0,1,
                [],['expand','shrink','fill'], 0,0);

my $hadj = Gtk2::Adjustment->new (100, 0, 300, 1, 10, 100);
my $hscroll = Gtk2::HScrollBar->new ($hadj);
$table->attach ($hscroll, 0,1, 1,2,
                ['expand','shrink','fill'],[], 0,0);

my $dragger;
my $confine = 0;
my $hinverted = 0;
my $vinverted = 0;
my $update_policy = 0;
sub make {
  $dragger = Gtk2::Ex::Dragger->new (widget        => $area,
                                     hadjustment   => $hadj,
                                     vadjustment   => $vadj,
                                     hinverted     => $hinverted,
                                     vinverted     => $vinverted,
                                     update_policy => $update_policy,
                                     confine       => $confine,
                                     cursor        => 'fleur');
  print __FILE__,($confine?"confined ":"unconfined "),
    ($hinverted?"hinv ":"hnorm "),
      ($vinverted?"vinv":"vnorm"),
        "policy $update_policy\n";
}
make();

{
  my $button = Gtk2::CheckButton->new_with_label ('Confine');
  $vbox->pack_start ($button, 0,0,0);
  $button->signal_connect (toggled => sub {
                             $confine = $button->get_active;
                             make();
                           });
}
{
  my $button = Gtk2::CheckButton->new_with_label ('H Inverted');
  $vbox->pack_start ($button, 0,0,0);
  $button->signal_connect (notify => sub {
                             $hinverted = $button->get_active;
                             make();
                           });
}
{
  my $button = Gtk2::CheckButton->new_with_label ('V Inverted');
  $vbox->pack_start ($button, 0,0,0);
  $button->signal_connect (notify => sub {
                             $vinverted = $button->get_active;
                             make();
                           });
}
{
  my $combobox = Gtk2::ComboBox->new_text;
  $vbox->pack_start ($combobox, 0,0,0);
  foreach my $policy ('default', 'continuous', 'discontinuous', 'delayed') {
    $combobox->append_text ($policy);
  }
  $combobox->set_active (0);
  $combobox->signal_connect
    (changed => sub {
       $update_policy = $combobox->get_active_text;
       make();
     });
}
{
  my $button = Gtk2::CheckButton->new_with_label ('Hint Mask');
  $vbox->pack_start ($button, 0,0,0);
  $button->signal_connect
    (notify => sub {
       $area->unrealize;
       my $motion_mask = $button->get_active
         ? ['pointer-motion-hint-mask'] : [];
       my $new_mask = $area->get('events')
         - 'pointer-motion-hint-mask'
           + $motion_mask;
       $area->set(events => $new_mask);
       print __FILE__,": area widget events ",$area->get('events'),"\n";
       $area->show;
       $area->map;
       my ($width, $height) = $area->window->get_size;
       print __FILE__,": area ${width}x${height} window events ",$area->window->get_events,"\n";

       $update_policy = 'continuous';
       make();
     });
}

{
  my $label = Gtk2::Label->new ('Keys:
Up,Down,Left,Right,
PgUp, PgDown');
  $vbox->pack_start ($label, 0,0,0);

  $area->signal_connect
    (key_press_event => sub {
       my ($area, $event) = @_;
       if ($event->keyval == Gtk2::Gdk->keyval_from_name('Page_Down')) {
         $vadj->set_value (min ($vadj->upper - $vadj->page_size,
                                $vadj->value + $vadj->page_increment));

       } elsif ($event->keyval == Gtk2::Gdk->keyval_from_name('Page_Up')) {
         $vadj->set_value (max ($vadj->lower,
                                $vadj->value - $vadj->page_increment));

       } elsif ($event->keyval == Gtk2::Gdk->keyval_from_name('Down')) {
         $vadj->set_value (min ($vadj->upper - $vadj->page_size,,
                                $vadj->value + $vadj->step_increment));

       } elsif ($event->keyval == Gtk2::Gdk->keyval_from_name('Up')) {
         $vadj->set_value (max ($vadj->lower,
                                $vadj->value - $vadj->step_increment));


       } elsif ($event->keyval == Gtk2::Gdk->keyval_from_name('Left')) {
         $hadj->set_value (min ($hadj->upper - $hadj->page_size,
                                $hadj->value + $hadj->step_increment));

       } elsif ($event->keyval == Gtk2::Gdk->keyval_from_name('Right')) {
         $hadj->set_value (max ($hadj->lower,
                                $hadj->value - $hadj->step_increment));

       }
       return 0; # propagate
     });
}

$area->add_events ('button-press-mask');
$area->signal_connect (button_press_event =>
                       sub {
                         my ($widget, $event) = @_;
                         print __FILE__.": start button $widget\n";
                         $dragger->start ($event);
                         return 0; # propagate
                       });

$toplevel->show_all;
Gtk2->main;
