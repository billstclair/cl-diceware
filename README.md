# cl-diceware

This is a Common Lisp implementation of the Diceware passphrase generation algorithm.

You can find a description of the algorithm at http://diceware.com

MIT license in LICENSE, and at the bottom of source files.

It's a lot more effort to get cryptographically secure random numbers in Windows, so I didn't bother.

Pretty standard lisp package. System definition in cl-diceware.asd. 

Tested in Clozure Common Lisp (CCL) and Steel Bank Common Lisp (SBCL).

## Command-line script

The "diceware" file is a bash script to print passphrases in a shell.

;; Run in CCL (assuming "ccl" will start the lisp).<br/>
`./diceware [<count>]`

;; Run in SBCL<br/>
`LISP=sbcl ./diceware [<count>]`

;; Run in other lisp<br/>
`LISP=whatever LISP_OPTIONS="--noprint options" ./diceware [<count>]`

&lt;count&gt; defaults to 5. Non-integer <count>, e.g. "-h" or "--help", prints help.

## Function reference

All functions are exported from the CL-DICEWARE package. Random bytes are fetched from /dev/random, except on Windows, where it uses *cl:random*.

**with-/dev/random** (&optional _stream_) &body _body_

> A macro to bind an (unsigned-byte 8) stream to STREAM around the execution of BODY. Wrap calls to the RANDOM-xxx functions with this to prevent multiple opening and closing of /dev/random.

**random-byte**

> Returns a random integer between 0 and 255 (inclusive).

**random-integer** _limit_

> Returns a random integer >= 0 and < limit. Same as **cl:random**, but better randomness.

**random-word**

> Returns a random word from the Diceware word list.

**random-words** _count_

> Returns a list of COUNT random words from the Diceware word list.

**random-words-string** _count_

>Returns a string containing COUNT random words, separated by spaces.

Bill St. Clair &lt;billstclair@gmail.com&gt;<br/>
4 September 2015
