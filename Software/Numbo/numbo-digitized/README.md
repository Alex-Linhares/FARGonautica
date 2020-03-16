# Numbo source, digitized

This directory contains a subset of the original source files for Numbo,
as digitized from [this printout](../numbo.Daniel.Defays.1987.pdf).

## about the digitization process

The digitization took place through three passes: a rough automatic OCR
pass, then two cleanup passes by a human. These aimed to preserve the
source *as it was*, for reference purposes, without modernizing it or
otherwise removing its idiosyncrasies.

That said, a few exceptions and judgement calls are worth mentioning:

-   to aid in cross-referencing between the source files and the scanned
    pages, the page headers were included in the source files as garish
    banners:

    ``` {.lisp}
    ;;
    ;; Jun 24 08:56 1987  init.l Page 3
    ;;
    ```

-   some scanned pages contain overstruck or otherwise superfluous
    characters — especially 'o' and 'e' in `codelets.l`. These have
    been removed.

-   a few lines ran off the right side of the scanned page — for
    instance, `read-target` in `codelets.l`. These have been
    reconstructed from context.

## does it run?

Unfortunately, no.

First, the source code relies on features of a now-unavailable Common
Lisp implementation. For Numbo to run on a typical contemporary Lisp
implementation, the graphics code would need to be ported to a
cross-platform library, and the use of
[Flavors](https://en.wikipedia.org/wiki/Flavors_(programming_language))
should be ported to
[CLOS](https://en.wikipedia.org/wiki/Common_Lisp_Object_System). These
are similar changes to those needed by
[Copycat](https://melaniemitchell.me/ExplorationsContent/how-to-get-copycat.html).

Second, the source files in the scan are not sufficient for
reconstructing the whole program. At the very least, the
coderack-specific code appears to be missing (`cr-hang`, `cr-choose`,
etc.).
