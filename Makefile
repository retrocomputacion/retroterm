default: all

BUILDFOLDER:=build
SRCFOLDER:=source

CBMVARIANTS=cbm-u cbm-sl cbm-ulti cbm-232
MSXVARIANTS=232 56k 38k bd

DEFINES_FOR_cbm-u=-D_HARDTYPE_=56
DEFINES_FOR_cbm-sl=-D_HARDTYPE_=38
DEFINES_FOR_cbm-ulti=-D_HARDTYPE_=1541
DEFINES_FOR_cbm-232=-D_HARDTYPE_=232
DEFINES_FOR_232=-E IFACE=0
DEFINES_FOR_bd=-E IFACE=1
DEFINES_FOR_56k=-E IFACE=56
DEFINES_FOR_38k=-E IFACE=38



# Create build directories
$(BUILDFOLDER) $(BUILDFOLDER)/cbm $(BUILDFOLDER)/cbm/packed $(BUILDFOLDER)/msx:
	mkdir -p $@

# Read build version from source/version.txt
VERSION := $(shell cat $(SRCFOLDER)/version.txt)

define make_target_acme
$(BUILDFOLDER)/cbm/rt-$(1).prg: $(SRCFOLDER)/retroterm_univ.asm $(SRCFOLDER)/version.asm | $(BUILDFOLDER)/cbm $(BUILDFOLDER)/cbm/packed
	acme $$(DEFINES_FOR_$(1)) -D_MAKE_=1 -I $(SRCFOLDER) -f cbm -l $(BUILDFOLDER)/cbm/rt-$(1).lst -o $$@ $$<
	exomizer sfx basic -o $(BUILDFOLDER)/cbm/packed/$$(@F) $$@

endef

define make_target_pasmo
$(BUILDFOLDER)/msx/rt-$(1).com: $(SRCFOLDER)/retrotermm1.asm $(SRCFOLDER)/retrologo.mseq $(SRCFOLDER)/version-msx.asm | $(BUILDFOLDER)/msx
	pasmo $$(DEFINES_FOR_$(1)) -I $(SRCFOLDER) $(SRCFOLDER)/retrotermm1.asm $(BUILDFOLDER)/msx/rt-$(1).com $(BUILDFOLDER)/msx/rtm-$(1).symbol

endef

# Main targets
# all: $(SRCFOLDER)/version.asm $(foreach variant,$(CBMVARIANTS),$(BUILDFOLDER)/rt-$(variant).prg) plus4 \
#      $(foreach variant,$(MSXVARIANTS),$(BUILDFOLDER)/rt-$(variant).com)
all: version cbm msx

cbm: $(SRCFOLDER)/version.asm $(foreach variant,$(CBMVARIANTS),$(BUILDFOLDER)/cbm/rt-$(variant).prg) plus4

msx: $(SRCFOLDER)/version-msx.asm $(foreach variant,$(MSXVARIANTS),$(BUILDFOLDER)/msx/rt-$(variant).com)

swiftlink: $(BUILDFOLDER)/cbm/rt-cbm-sl.prg

ultimate: $(BUILDFOLDER)/cbm/rt-cbm-ulti.prg

turbo232: $(BUILDFOLDER)/cbm/rt-cbm-232.prg

userport: $(BUILDFOLDER)/cbm/rt-cbm-u.prg

plus4: $(BUILDFOLDER)/cbm/rt-cbm-p4.prg

msx232: $(BUILDFOLDER)/msx/rt-msx-232.com

msxbadcat: $(BUILDFOLDER)/msx/rt-msx-bd.com

msx56k: $(BUILDFOLDER)/msx/rt-msx-56k.com

msx38k: $(BUILDFOLDER)/msx/rt-msx-38k.com

version: $(SRCFOLDER)/version.asm $(SRCFOLDER)/version-msx.asm

# Special case for p4 (uses different source file)
$(BUILDFOLDER)/cbm/rt-cbm-p4.prg: $(SRCFOLDER)/retrotermp4.asm | $(BUILDFOLDER)/cbm $(BUILDFOLDER)/cbm/packed
	acme -f cbm -D_MAKE_=1 -I $(SRCFOLDER) -l $(BUILDFOLDER)/rt-cbm-p4.lst -o $(BUILDFOLDER)/cbm/rt-cbm-p4.prg $(SRCFOLDER)/retrotermp4.asm
	exomizer sfx basic -t4 -o $(BUILDFOLDER)/cbm/packed/rt-cbm-p4.prg $(BUILDFOLDER)/cbm/rt-cbm-p4.prg

# Generate version includes
$(SRCFOLDER)/version.asm: $(SRCFOLDER)/version.txt | $(SRCFOLDER)
	echo !text '"'$(VERSION)'"' > $(SRCFOLDER)/version.asm
$(SRCFOLDER)/version-msx.asm: $(SRCFOLDER)/version.txt | $(SRCFOLDER)
	echo DB '"'$(VERSION)'"' > $(SRCFOLDER)/version-msx.asm

# Generate rules dynamically
$(eval $(foreach variant,$(CBMVARIANTS),$(call make_target_acme,$(variant))))
$(eval $(foreach variant,$(MSXVARIANTS),$(call make_target_pasmo,$(variant))))

# Clean target
clean:
	rm -rf $(BUILDFOLDER)