# Mode for JunOS configuration files

This features `junos-mode` a mode for editing JunOS-like files.

There is currently no configuration knob.

![Screenshot of junos-mode](screenshot.jpg)

## Integration with org-mode and Babel

It also comes with an integration with org-mode and Babel. It uses a
small Python helper to efficiently handle several parallel sessions
asynchronously. This helper
uses [Junos PyEZ](https://github.com/Juniper/py-junos-eznc).

    #+BEGIN_SRC junos :host alfred.exoscale.local
    system {
        time-zone Europe/Paris;
    }
    #+END_SRC
    
    #+RESULTS:
    #+begin_example
    Load replace: ✓
    Checks: ✓
    
    Differences
    ‾‾‾‾‾‾‾‾‾‾‾
       [edit system]
       -  time-zone Europe/Zurich;
       +  time-zone Europe/Paris;
       
    #+end_example
    
There are some limitations with this integration. Notably, it expects
the output of the execution will be in a block, like above. If you
force another representation, this won't work as expected.

This is only compatible with with Org 9. It also requires
the [UUID module](https://github.com/nicferrier/emacs-uuid).

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
