;---------------------------------------------
; WORKSPACE-STRUCTURES: This file contains flavor definitions and functions 
;                        for workspace structures.  These include 
;                        descriptions, bonds, groups, replacements, and 
;                        correspondences.  
;---------------------------------------------

(in-package 'user)

(defflavor workspace-structure 
    (string ; The string the structure is in.
     structure-category ; E.g., 'bond or 'group.
     (group nil)  ; T if the structure (e.g., a bond) is inside a group.
     (internal-strength 0) (external-strength 0) (total-strength 0)
     proposal-level graphics-obj)
    ()
    :gettable-instance-variables
    :settable-instance-variables
    :initable-instance-variables)

