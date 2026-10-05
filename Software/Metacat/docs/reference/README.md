# Reference: Marshall's dissertation and its figures

This folder holds the primary source behind Metacat. It has James B. Marshall's
dissertation as a PDF, and every screenshot and diagram in it extracted as a PNG and indexed
below by the window (panel) it shows. The Racket and Python ports use the dissertation in
two ways. It explains what the code is meant to do where the code is unclear. Its figures
show what each window should look like, and the "look at what you draw" steps of both Ralph
loops compared every rendered panel against them. For run-by-run differences between the
dissertation and the program, see [`../demos.md`](../demos.md). For pictures of the ports
themselves, see [`../screenshots/`](../screenshots/README.md).

## Contents

| Path | What |
| --- | --- |
| `dissertation.pdf` | James B. Marshall, *Metacat: A Self-Watching Cognitive Architecture for Analogy-Making and High-Level Perception*, Indiana University, 1999; 306 pages. Fetched on 2026-10-03 from <https://science.slc.edu/~jmarshall/metacat/dissertation.pdf> (loop0001 item 12). |
| `figures/` | 197 PNGs: every raster image in the PDF larger than 100 × 100 pixels. |

The dissertation is Marshall's work. It is included here as a reference, and its home is
the [Metacat home page](https://science.slc.edu/~jmarshall/metacat/).

## How the figures were extracted

`figures/` holds every raster image in the PDF larger than 100 × 100 pixels (197
of them, greyscale, about 100–200 dpi), extracted unchanged with
`pdfimages -png -p dissertation.pdf` and named `pPPP-NNN.png` (PDF page,
image number). Smaller images (rule boxes, stray glyphs) were left out. The
screenshots come from the 1999 program (Chez Scheme + SchemeXM/SGL on X), so
they show the SGL drawings that `sgl-interpreter.ss` reproduces on Tk.

To read a figure's caption, use `pdftotext -f P -l P dissertation.pdf -`, where P is the
page number from the file name. For example, `p224-538.png` is on PDF page 224.

## Figures by panel

| Panel (original file) | Figures |
| --- | --- |
| Workspace (workspace-graphics.ss, bridge-, group-, rule-graphics.ss) | p077-093 (Fig. 2.4), p079-096 (2.5), p081-099 (2.6), p093-152 (3.1), p131-243 … p141-268 (3.11–3.18), p173-319 (4.9), p224-541, p228-574, p231-604, p235-642, p239-668, p241-682, p256-827, p280-1049, p282-1059, p282-1065; workspace fragments: p120-224 … p128-238 (3.5–3.10) |
| Workspace in the Temporal Trace's event views ("Event N: …") | p224-547, p225-554, p226-565, p228-580, p231-598, p232-611, p233-621, p234-635, p238-655, p238-661, p239-674, p242-692, p244-711, p245-718, p246-726, p246-732, p248-745 … p262-879 |
| Answer Description (memory.ss, workspace graphics) | p203-388 (4.16), p229-589, p284-1079, p285-1087 |
| Slipnet (slipnet-graphics.ss); Copycat's version | p040-042 (1.2) |
| Concept patterns (slipnet layout: Top Rule Concepts, Group Descriptors, Concept Pattern) | p167-307, p167-308 (4.7), p226-568, p233-624 |
| Coderack (coderack-graphics.ss) | p047-054, p047-055 (Copycat, 1.5), p169-311, p169-312 (4.8), p237-649, p237-650 |
| Themespace: Top / Bottom / Vertical Themes (theme-graphics.ss) | p150-280 (4.1), p152-283 (4.2), p163-300 (4.5), p165-304 (4.6), p174-321 (4.10), p203-387, p203-389, p226-560 … p226-562, p229-584 … p229-586, p233-616 … p233-618, p255-818, p261-861, p261-868, p262-876, p280-1046, p282-1056, p282-1062, p284-1076 |
| Temporal Trace (trace-graphics.ss) | p180-330 (4.12), p181-332, p181-333 (4.13), p224-538, p224-544, p225-551, p226-559, p228-571, p228-577, p231-595, p231-601, p232-608, … p285-1083 (every wide strip with a title bar "Temporal Trace") |
| Commentary (commentary-graphics.ss) | p195-354, p195-355 (4.14, windows), p199-364 … p199-379 (4.15), p243-698, p247-738, p262-885, p265-894 … p271-996 (answer comparisons, Chapter 5) |
| Episodic Memory (memory-graphics.ss) | p215-470 (4.17) |
| Not panels: Copycat run summary bar chart, theme-strength curve | p045-050 (1.4), p158-292 (4.4) |
| Workspace for Copycat's Workspace and snag (Fig. 1.3, 2.1) | p041-044, p058-073 |
| Workspace at rule-level detail (Chapter 5 convoluted rules) | p273-1003, p273-1008, p274-1014, p274-1019 |

No figure shows the Temperature thermometer or the EEG window by
themselves.

## Useful starting points

- File names use **PDF page numbers**. The dissertation's printed page numbers are 16
  lower on these pages: PDF page 224 is printed page 208. [`../demos.md`](../demos.md)
  quotes the dissertation's own page numbers ("p. 240").
- **Chapter 5, "Sample Runs of the Program"**, starts on PDF page 223. Its Runs 1–8 are
  checked against the oracle in [`../demos.md`](../demos.md). The screenshots on PDF page
  224 (`p224-538.png` … `p224-547.png`) are the panels of Run 1 (`abc → abd; mrrjjj →
  mrrjjjj`) after about 500 codelets. Loop0001 compared them with the Racket port's
  `abc abd mrrjjj` Workspace after 513 codelets
  ([`../screenshots/mrrjjj-513-workspace.png`](../screenshots/mrrjjj-513-workspace.png)).
