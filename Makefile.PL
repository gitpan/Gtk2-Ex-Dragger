#!/usr/bin/perl

# Copyright 2007, 2008, 2009, 2010 Kevin Ryde

# This file is part of Gtk2-Ex-Dragger.
#
# Gtk2-Ex-Dragger is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 3, or (at your option) any
# later version.
#
# Gtk2-Ex-Dragger is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
# Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with Gtk2-Ex-Dragger.  If not, see <http://www.gnu.org/licenses/>.

use 5.008;
use strict;
use warnings;
use ExtUtils::MakeMaker;
use lib 'inc';
use MyMakeMakerExtras;

MyMakeMakerExtras::WriteMakefile
  (NAME         => 'Gtk2-Ex-Dragger',
   ABSTRACT     => 'Mouse drag to move widget contents.',
   VERSION_FROM => 'lib/Gtk2/Ex/Dragger.pm',
   AUTHOR       => 'Kevin Ryde <user42@zip.com.au>',
   LICENSE      => 'gpl',
   SIGN         => 1,
   MIN_PERL_VERSION => '5.008',

   PREREQ_PM    => {
                    # version 1.200 for GDK_CURRENT_TIME,
                    # GDK_PRIORITY_REDRAW, and various bug fixes
                    'Gtk2' => '1.200',

                    # from my Glib-Ex-ObjectBits
                    'Glib::Ex::SignalIds' => 0,

                    # version 3 split out to WidgetBits
                    'Gtk2::Ex::SyncCall' => 3,
                   },

   META_MERGE
   => { resources
        => { homepage => 'http://user42.tuxfamily.org/gtk2-ex-dragger/index.html' },
        optional_features
        => { cursor
             => { description => 'Setting a different mouse cursor while dragging.',
                  requires => { 'Gtk2::Ex::WidgetCursor' => 0 }
                },

             maximum_examples
             => { description => 'Be able to run all the example programs.',
                  requires => { 'Gtk2::Ex::WidgetCursor'  => 0,
                              },
                },
             maximum_tests
             => { description => 'Have "make test" do as much as possible.',
                  requires => { 'Test::NoWarnings' => 0,
                              },
                },
           },
      },
  );
