memoria
=======

![Build Status](https://github.com/mpilgrem/memoria/actions/workflows/tests.yml/badge.svg)
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![Haskell](https://img.shields.io/badge/Haskell-5e5086?logo=haskell&logoColor=white)](http://haskell.org)

Originally forked from [`memory-0.18.0`](https://hackage.haskell.org/package/memory-0.18.0).

Documentation: [memory on hackage](http://hackage.haskell.org/package/memory)

A generic memory and related abstraction for haskell:

* A polymorphic byte array abstraction and function similar to strict ByteString.
* Different type of byte array abstraction.
* Raw memory IO operations (memory set, memory copy, ..)
* Aliasing with endianness support.

Also provides some useful helpers:

* Fast Hashing : [SipHash](https://131002.net/siphash/), [FNV1](http://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function).
* Built-in base encoding : Base16, Base32, [Base64](http://en.wikipedia.org/wiki/Base64).

Versioning
----------

Development versions are an incremental number prefixed by 0.
No specific meaning is associated with the versions, specially
no API stability.

Production versions : TBD

Coding Style
------------

The coding style of this project mostly follows:
[haskell-style](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md)

Support
-------

See [Haskell packages guidelines](https://github.com/vincenthz/haskell-pkg-guidelines/blob/master/README.md#support)

History
-------

The [`memory`](https://hackage.haskell.org/package/memory) package was
originated and then maintained by Vincent Hanquez. Nicholas Di Prima joined as
a co-maintainer from version 0.14.6. For published reasons, they do not intend
to develop the package further after version 0.18.0 but Vincent Hanquez also
does not want to introduce other maintainers.
