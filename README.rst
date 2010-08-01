binary-protocol-zmq
===================
This package contains a monad to ease development of binary network protocols
on top of ZeroMQ_ sockets.

The protocol should be implemented using messages implementing the Binary_
class. This code is heavily based on the binary-protocol_ package by
Gregory Crosswhite, and mimicks its API.

You can build the API documentation using `cabal haddock`.

.. _ZeroMQ: http://www.zeromq.org
.. _Binary: http://hackage.haskell.org/package/binary
.. _binary-protocol: http://hackage.haskell.org/package/binary-protocol
