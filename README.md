This is a Common Lisp implementation of the Diceware passphrase generation algorithm.

You can find a description of the algorithm at http://diceware.com

MIT license in LICENSE, and at the bottom of source files.

It's a lot more effort to get cryptographically secure random numbers in Windows, so I didn't bother.

Pretty standard lisp package. System definition in cl-diceware.asd. 

The "diceware" file is a bash script to run it. More info there.

Tested in Clozure Common Lisp (CCL) and Steel Bank Common Lisp (SBCL).

;; Run in CCL (assuming "ccl" will start the lisp).<br/>
`./diceware [<count>]`

;; Run in SBCL<br/>
`LISP=sbcl ./diceware [<count>]`

;; Run in other lisp<br/>
`LISP=whatever LISP_OPTIONS="--noprint options" ./diceware [<count>]`

&lt;count&gt; defaults to 5. Non-integer <count>, e.g. "-h" or "--help", prints help.

Bill St. Clair &lt;billstclair@gmail.com&gt;<br/>
5 September 2015
