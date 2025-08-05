Command line parameters
~~~~~~~~~~~~~~~~~~~~~~~

| Input - text files of modules with the extension **.c07**
| Output - executable file of the ELF64 format

Parameters:

1. name of the main module
2. application type

   -  linux64exe - Linux ELF64-EXEC
   -  linux64so - Linux ELF64-SO

3. optional parameters-keys

   -  out - name of the resulting file; by default, same as main module
      name,
      but with different extension (corresponds to executable file type)
   -  stk - stack size in megabytes (default **2** Mb, allowed from 1 to
      32 Mb)
   -  tab - tab size (used to calculate coordinates in source code),
      default **4**
   -  nochk - disable runtime checks
   -  lower - allow lowercase keywords and built-in identifiers
      (default)
   -  upper - only uppercase for keywords and built-in identifiers
   -  def - set conditional compilation symbol
   -  uses - list imported modules

The -nochk parameter is specified as a string of characters:

-  p - pointers
-  t - types
-  i - indices
-  b - implicit cast of INTEGER to BYTE
-  c - range of CHR function argument
-  w - range of WCHR function argument
-  r - equivalent to bcw
-  a - all checks

| The order of symbols can be any. The presence of a particular symbol
  in
| the line disables the corresponding check.

Differences from the original
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1.  The SYSTEM pseudo-module has been expanded
2.  The underscore character is allowed in identifiers
3.  System flags have been added
4.  The CASE operator has been improved
    (constant expressions have been added to variant labels and an
    optional ELSE branch)
5.  The set of standard procedures has been expanded
6.  The semantics of the type check have been clarified for a null
    pointer
7.  Single-line comments have been added (they start with a pair of
    characters **//**)
8.  Inheritance from a pointer type is allowed
9.  Syntax for importing procedures from external libraries has been
    added
10. Strings can also be enclosed in single quotes
11. The WCHAR type has been added
12. The concatenation operation of string and symbolic constants has
    been added
13. It is possible to import modules by specifying the path and file
    name
14. Special syntax for conditional compilation has been added
15. The procedure name at the end of the declaration (after END) is
    optional
16. It is allowed to use lowercase for keywords

Implementation Features
~~~~~~~~~~~~~~~~~~~~~~~

1. Basic Types

======= =========================================== ===========
Type    Range of values                             Size, bytes
======= =========================================== ===========
INTEGER -9223372036854775808 .. 9223372036854775807 8
REAL    4.94E-324 .. 1.70E+308                      8
CHAR    symbol ASCII (0X .. 0FFX)                   1
BOOLEAN FALSE, TRUE                                 1
SET     set of integers {0 .. 63}                   8
BYTE    0 .. 255                                    1
WCHAR   unicode symbol (0X .. 0FFFFX)               2
======= =========================================== ===========

2.  Maximum length of identifiers is **255** characters
3.  Maximum length of string constants is **511** characters (UTF-8)
4.  Maximum dimension of open arrays is **5**
5.  The NEW procedure fills the allocated memory block with zeros
6.  Global and local variables are initialized to zeros
7.  Unlike many Oberon implementations, there is no garbage collector
    and dynamic modularity
8.  The BYTE type in expressions is always cast to INTEGER
9.  No overflow checking of expression values is performed
10. Run-time errors:

    -  ASSERT(x), when x = FALSE
    -  null pointer dereference
    -  integer division by a non-positive number
    -  procedure call through a procedure variable with a null value
    -  type guard error
    -  array bounds violation
    -  unexpected expression value in CASE statement
    -  array copy error v := x, if LEN(v) < LEN(x)
    -  CHR(x), if (x < 0) OR (x > 255)
    -  WCHR(x), if (x < 0) OR (x > 65535)
    -  implicit cast x:INTEGER to v:BYTE, if (x < 0) OR (x > 255)

Pseudo-module SYSTEM
~~~~~~~~~~~~~~~~~~~~

| The pseudo-module SYSTEM contains low-level and unsafe procedures,
| errors in using procedures of the pseudo-module SYSTEM can lead to
| corruption of run-time data and abnormal termination of the program.

::

   PROCEDURE ADR(v: any type): INTEGER
           v is a variable or procedure;
           returns the address of v

   PROCEDURE SADR(x: string constant (CHAR UTF-8)): INTEGER
           returns the address of x

   PROCEDURE WSADR(x: string constant (WCHAR)): INTEGER
           returns the address of x

   PROCEDURE VAL(T; v: any type): T
           v is a variable;
           interprets v as a variable of type T

   PROCEDURE SIZE(T): INTEGER
           returns the size of type T

   PROCEDURE TYPEID(T): INTEGER
           T is a record type or pointer type,
           returns the number of the type in the table of record types

   PROCEDURE INF(): REAL
           returns the special real value "infinity"

   PROCEDURE MOVE(Source, Dest, n: INTEGER)
           Copies n bytes of memory from Source to Dest,
           Source and Dest cannot overlap

   PROCEDURE GET(a: INTEGER; VAR v: any basic type, PROCEDURE, POINTER)
           v := Memory[a]

   PROCEDURE GET8(a: INTEGER; VAR x: INTEGER, SET, BYTE, CHAR, WCHAR, SYSTEM.CARD32)
           Equivalent to SYSTEM.MOVE(a, SYSTEM.ADR(x), 1)

   PROCEDURE GET16(a: INTEGER; VAR x: INTEGER, SET, WCHAR, SYSTEM.CARD32)
           Equivalent to SYSTEM.MOVE(a, SYSTEM.ADR(x), 2)

   PROCEDURE GET32(a: INTEGER; VAR x: INTEGER, SET, SYSTEM.CARD32)
           Equivalent to SYSTEM.MOVE(a, SYSTEM.ADR(x), 4)

   PROCEDURE PUT(a: INTEGER; x: any basic type, PROCEDURE, POINTER)
           Memory[a] := x;
           If x: BYTE or x: WCHAR, then the value of x will be extended
           to 64 bits, to write bytes use SYSTEM.PUT8,
           for WCHAR -- SYSTEM.PUT16

   PROCEDURE PUT8(a: INTEGER; x: INTEGER, SET, BYTE, CHAR, WCHAR, SYSTEM.CARD32)
           Memory[a] := lower 8 bits (x)

   PROCEDURE PUT16(a: INTEGER; x: INTEGER, SET, BYTE, CHAR, WCHAR, SYSTEM.CARD32)
           Memory[a] := lower 16 bits (x)

   PROCEDURE PUT32(a: INTEGER; x: INTEGER, SET, BYTE, CHAR, WCHAR, SYSTEM.CARD32)
           Memory[a] := lower 32 bits (x)

   PROCEDURE COPY(VAR Source: any type; VAR Dest: any type; n: INTEGER)
           Copies n bytes of memory from Source to Dest.
           Equivalent to SYSTEM.MOVE(SYSTEM.ADR(Source), SYSTEM.ADR(Dest), n)

   PROCEDURE CODE(byte1, byte2,... : BYTE)
           Inserting machine code,
           byte1, byte2 ... - constants in the range 0..255,
           for example: SYSTEM.CODE(048H,08BH,045H,010H) (* mov rax,qword[rbp+16] *)

| Also, the SYSTEM module defines the CARD32 type (**4** bytes).
| No explicit operations are allowed for the CARD32 type, except for
  assignment.

The functions of the SYSTEM pseudo-module cannot be used in constant
expressions.

System flags
~~~~~~~~~~~~

| When declaring procedural types and global procedures, a calling
  convention flag
| may be specified after the PROCEDURE keyword: ``[systemv]``,
  ``[linux]``, ``[oberon]``, ``[ccall]``.

For example:

::

   PROCEDURE [oberon] MyProc (x, y, z: INTEGER): INTEGER;

| The ``[linux]`` flag is a synonym for ``[systemv]``. The ``[ccall]``
  flag is a synonym for ``[systemv]``.
| A dash after the flag name (``[linux-]``) means that the procedure
  result
| may be ignored (not allowed for the REAL type). If the flag is not
  specified
| or the ``[oberon]`` flag is specified, the internal calling convention
  is assumed.
| ``[systemv]`` is used for communication with the operating system and
  external applications.

| When declaring record types, the ``[noalign]`` flag can be specified
  after the RECORD keyword.
| The ``[noalign]`` flag means no alignment of the record fields.
| Records with the system flag cannot have a base type and cannot be the
  base type of other records.

You must import SYSTEM to use system flags.

CASE statement
~~~~~~~~~~~~~~

CASE statement syntax:

::

   CaseStatement =
           CASE Expression OF Case {"|" Case}
                   [ELSE StatementSequence] END.
   Case = [CaseLabelList ":" StatementSequence].
   CaseLabelList = CaseLabels {"," CaseLabels}.
   CaseLabels = ConstExpression [".." ConstExpression].

For example:

::

   CASE x OF
   |-1:    DoSomething1
   | 1:    DoSomething2
   | 0:    DoSomething3
   ELSE
           DoSomething4
   END

| Constant expressions can be used in variant labels, the ELSE branch is
  optional.
| If the value of x does not correspond to any variant and ELSE is
  missing, the program
| terminates with a run-time error.

WCHAR Type
~~~~~~~~~~

| The WCHAR type has been added to the language for convenient Unicode
  support.
| For WCHAR and ARRAY OF WCHAR types, all the same operations are
  allowed as for CHAR and
| ARRAY OF CHAR types, with the exception of the built-in CHR procedure,
  which
| returns only the CHAR type. To obtain a value of the WCHAR type,
| you should use the WCHR procedure instead of CHR. To work correctly
  with the type,
| you must save the source code in UTF-8 encoding with BOM.

Concatenation of string and character constants
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Concatenation of constant strings and CHAR characters is allowed:

::

   str = CHR(39) + "string" + CHR(39); (* str = "'string'" *)

   newline = 0DX + 0AX;

Null pointer type check and guard
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

| The original language announcement does not define the program’s
  behavior when
| guarding p(T) and checking the type of p IS T when p = NIL.
| In many Oberon implementations, performing such an operation results
  in a runtime error.
| In this implementation, the null pointer type guard does not result in
  an error,
| and the type check yields FALSE. In some cases, this allows to
  significantly
| reduce the frequency of using type guards.

Additional Standard Procedures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   DISPOSE (VAR v: any_pointer)
           Frees the memory allocated by the NEW procedure for the
           dynamic variable v^, and assigns NIL to the variable v.

   COPY (x: ARRAY OF CHAR/WCHAR; VAR v: ARRAY OF CHAR/WCHAR);
           v := x;
           If LEN(v) < LEN(x), then the string x will not be copied completely

   LSR (x, n: INTEGER): INTEGER
           Logical shift of x n bits to the right.

   MIN (a, b: INTEGER): INTEGER
           Minimum of two values.

   MAX (a, b: INTEGER): INTEGER
           Maximum of two values.

   BITS (x: INTEGER): SET
           Interprets x as a value of type SET.
           Executed at compile time.

   LENGTH (s: ARRAY OF CHAR/WCHAR): INTEGER
           Length of 0X-terminated string s, excluding the 0X character.
           If the 0X character is missing, the function returns the length
           of array s. s cannot be a constant.

   WCHR (n: INTEGER): WCHAR
           Type conversion similar to CHR(n: INTEGER): CHAR

Importing modules with path and file name
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Examples:

::

   IMPORT Math IN "./lib/math.ob07"; (* relative to the current module *)

   IMPORT M1 IN "C:\lib\math.ob07"; (* absolute path *)

Hidden parameters of procedures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

| Some procedures may have hidden parameters, they are not in the list
  of
| formal parameters, but are taken into account by the compiler
| when translating calls. This is possible in the following cases:

1. The procedure has a formal parameter open array:

::

   PROCEDURE Proc (x: ARRAY OF ARRAY OF REAL);
           The call is translated as follows:
           Proc(LEN(x), LEN(x[0]), SYSTEM.ADR(x))

2. The procedure has a formal parameter-variable of type RECORD:

::

   PROCEDURE Proc (VAR x: Rec);
           The call is translated as follows:
           Proc(SYSTEM.TYPEID(Rec), SYSTEM.ADR(x))

Hidden parameters must be taken into account when communicating with
external applications.

RTL Module
~~~~~~~~~~

| All programs implicitly use the RTL module. The compiler translates
| some operations (type checking and guarding, string comparison,
  run-time
| error messages, etc.) as calls to procedures in this module.
| You should not call these procedures explicitly.
| Run-time error messages are printed to the terminal (Linux).

API Module
~~~~~~~~~~

| Like the RTL module, the API module is not intended for direct use.
| It provides communication between the RTL and the OS.

Generating DLL executables
~~~~~~~~~~~~~~~~~~~~~~~~~~

| Only procedures can be exported. To do this, the procedure must
| be in the main module of the program, its name must be marked with the
| export symbol, and the calling convention must be specified. You
  cannot
| export procedures that are imported from other DLLs.
