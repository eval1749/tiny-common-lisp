# genesis/genesis.mk
#
# This file is part of Tiny CL project.
#
# Generate lisp.image for console
#
# Copyright (C) 2008 by Project Vogue
# Written by Yoshifumi Inoue (yosi@msn.com)
#
# @(#)$Id: //proj/evedit2/mainline/genesis/genesis.mk#5 $
#

GenesisDir = $(SolutionDir)lisp\genesis

image = $(OutDir)tinycl.image

edgen    = $(OutDir)\genesis.lisp
edimage  = $(OutDir)\editor.image
edmarker = $(OutDir)\ed_image_marker.txt

sources = \
    $(GenesisDir)\genesis.lisp \
    $(GenesisDir)\genesis-macro.lisp \
    $(GenesisDir)\genesis-regex.lisp \
    $(GenesisDir)\genesis-runtime.lisp

build: $(image) $(edgen) $(edmarker)

clean:
    del/s $(image) $(edimage)

$(image) : $(sources) $(edgen)
    cd $(OutDir)
    .\console.exe -genesis < genesis.lisp
    move console.image tinycl.image


$(edgen) : $(GenesisDir)\genesis.lisp
    copy $(GenesisDir)\genesis.lisp $(edgen)

$(edmarker) : \
        $(SolutionDir)editor\ed_layout.inc \
        $(SolutionDir)editor\ed_objects.inc
    date/t > $(edmarker)
    time/t >> $(edmarker)
    if exist $(edimage) del $(edimage)
