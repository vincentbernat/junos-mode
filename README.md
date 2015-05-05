# Mode for JunOS configuration files

This features `junos-mode` a mode for editing JunOS-like files.

There is currently no configuration knob.

![Screenshot of junos-mode](screenshot.jpg)

## Integration with Babel

It also comes with an integration with Babel. This integration is
quite basic and will send a configuration to the host and provide
output of `commit check` and `show | diff`. There is no support for
session. An inferior process is run for each provided host and will
not be killed automatically.

For example:

    #+BEGIN_SRC junos :host alfred.exoscale.local
    routing-instances {
        FW-CLOUD {
            routing-options {
                static {
                    route 0.0.0.0/0 next-hop 192.0.2.1;
                }
            }
        }
    }
    #+END_SRC

## License

> This file is free software; you can redistribute it and/or modify
> it under the terms of the GNU General Public License as published by
> the Free Software Foundation; either version 2, or (at your option)
> any later version.
>
> This file is distributed in the hope that it will be useful,
> but WITHOUT ANY WARRANTY; without even the implied warranty of
> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
> GNU General Public License for more details.
>
> You should have received a copy of the GNU General Public License
> along with GNU Emacs; see the file COPYING.  If not, write to
> the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
> Boston, MA 02111-1307, USA.
