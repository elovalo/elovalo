<!-- -*- mode: markdown; coding: utf-8 -*- -->

# Wire message examples

Here are some raw messages in ZCL "wire" format. All of these have
valid CRC and should return ACK (`K`). To test NAK handling, just
mutilate message bodies.

In the following messages the MAC address (unless stated otherwise) is
`EFCDAB8967452301`. It is sent in big endian format (the same ordering
used in *XML* format). Little-endian format is used on other parts of
message.

Valid message to an invalid MAC address:

    S01B000100000000006ACEB8460004000008000112000010011300001801D86A

[Read basic cluster](read.basic.cluster.xml):

    S01B0001EFCDAB89674523014600040000080001120000100113000018016391

[Read Elovalo cluster](read.elovalo.cluster.xml):

    S02B0001EFCDAB896745230146000400050800001200010002000300040005000600070008000900100011001300A61A
