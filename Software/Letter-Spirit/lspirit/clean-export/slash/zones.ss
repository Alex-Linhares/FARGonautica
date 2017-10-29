Zones:
topbox ascender bottombox descender rightedge leftedge topedge bottomedge

add general ones like top and bottom, left and right

We can use the function (anyone? ls) to see if any quanta in ls are in
*quanta-list*

(set! *topedge-zone* '(0 1))
(set! *topbox-zone* '(0 1 14 32 44 15 33 45 16))
(set! *ascender-zone* '(0 1 2 3 14 32 44 15 33 45 16 17 34 46 18 35 47 19))
(set! *descender-zone*
      '(12 13 29 42 54 30 43 55 31 10 11 26 40 52 27 41 53 28))
(set! *bottombox-zone* '(12 13 29 42 54 30 43 55 31))
(set! *bottomedge-zone* '(12 13))
(set! *rightedge-zone* '(16 19 22 25 28 31))
(set! *leftedge-zone* '(14 17 20 23 26 29))
(set! *vertedges-zone* '(0 1 12 13))
(set! *vertboxes-zone* '(0 1 14 32 44 15 33 45 16 12 13 29 42 54 30 43 55 31))
(set! *topleft-zone* '(0 14 44))
(set! *topright-zone* '(1 16 33))
(set! *bottomleft-zone* '(29 42 54))
(set! *bottomright-zone* '(31 43 55))
(set! *corners-zone* '(0 14 44 1 16 33 29 42 54 31 43 55))
