# Lighthouse

BEAM monitoring

![Build Status](https://github.com/lpgauth/lighthouse/workflows/Erlang%20CI/badge.svg?branch=dev)

### Requirements

* Erlang 19.0 +

### Features

%% TODO

#### Environment variables:

<table width="100%">
  <theader>
    <th>Name</th>
    <th>Type</th>
    <th>Default</th>
    <th>Description</th>
  </theader>
  <tr>
    <td>backends</td>
    <td>backends()</td>
    <td>[lighthouse_statsderl]</td>
    <td>metric backends</td>
  </tr>
  <tr>
    <td>plugins</td>
    <td>plugins()</td>
    <td>[lighthouse_linux, lighthouse_msacc, lighthouse_vm]</td>
    <td>metric plugins</td>
  </tr>
</table>

## Tests

```makefile
make test
```

## License

The MIT License (MIT)

Copyright (c) 2020 Louis-Philippe Gauthier

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
