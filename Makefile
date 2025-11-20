BUILDFOLDER:=build
SRCFOLDER:=source

CBMVARIANTS=cbm-u cbm-sl cbm-ulti cbm-232
MSXVARIANTS=msx-232 msx-56k msx-38k

DEFINES_FOR_cbm-u=-D_HARDTYPE_=56
DEFINES_FOR_cbm-sl=-D_HARDTYPE_=38
DEFINES_FOR_cbm-ulti=-D_HARDTYPE_=1541
DEFINES_FOR_cbm-232=-D_HARDTYPE_=232
DEFINES_FOR_msx-232=-E IFACE=0
DEFINES_FOR_msx-56k=-E IFACE=56
DEFINES_FOR_msx-38k=-E IFACE=38

# Create build directories
$(BUILDFOLDER) $(BUILDFOLDER)/packed:
	mkdir -p $@

# Read build version from source/version.txt
VERSION:=$(file < $(SRCFOLDER)/version.txt)

define make_target_acme
$(BUILDFOLDER)/rt-$(1).prg: $(SRCFOLDER)/retroterm_univ.asm $(SRCFOLDER)/version.asm | $(BUILDFOLDER) $(BUILDFOLDER)/packed
	acme $$(DEFINES_FOR_$(1)) -D_MAKE_=1 -I $(SRCFOLDER) -f cbm -l $(BUILDFOLDER)/rt-$(1).lst -o $$@ $$<
	exomizer sfx basic -o $(BUILDFOLDER)/packed/$$(@F) $$@

endef

define make_target_pasmo
$(BUILDFOLDER)/rt-$(1).com: $(SRCFOLDER)/retrotermm1.asm $(SRCFOLDER)/retrologo.mseq
	pasmo $$(DEFINES_FOR_$(1)) -I $(SRCFOLDER) $(SRCFOLDER)/retrotermm1.asm $(BUILDFOLDER)/rt-$(1).com $(BUILDFOLDER)/rtm-$(1).symbol

endef

# Main targets
all: $(SRCFOLDER)/version.asm $(foreach variant,$(CBMVARIANTS),$(BUILDFOLDER)/rt-$(variant).prg) plus4 \
     $(foreach variant,$(MSXVARIANTS),$(BUILDFOLDER)/rt-$(variant).com)

cbm: $(SRCFOLDER)/version.asm $(foreach variant,$(CBMVARIANTS),$(BUILDFOLDER)/rt-$(variant).prg) plus4

msx: $(SRCFOLDER)/version.asm $(foreach variant,$(MSXVARIANTS),$(BUILDFOLDER)/rt-$(variant).com)

swiftlink: $(BUILDFOLDER)/rt-cbm-sl.prg

ultimate: $(BUILDFOLDER)/rt-cbm-ulti.prg

turbo232: $(BUILDFOLDER)/rt-cbm-232.prg

userport: $(BUILDFOLDER)/rt-cbm-u.prg

plus4: $(BUILDFOLDER)/rt-cbm-p4.prg

msx232: $(BUILDFOLDER)/rt-msx-232.com

msx56k: $(BUILDFOLDER)/rt-msx-56k.com

msx38k: $(BUILDFOLDER)/rt-msx-38k.com

# Special case for p4 (uses different source file)
$(BUILDFOLDER)/rt-cbm-p4.prg: $(SRCFOLDER)/retrotermp4.asm | $(BUILDFOLDER) $(BUILDFOLDER)/packed
	acme -f cbm -D_MAKE_=1 -I $(SRCFOLDER) -l $(BUILDFOLDER)/rt-cbm-p4.lst -o $(BUILDFOLDER)/rt-cbm-p4.prg $(SRCFOLDER)/retrotermp4.asm
	exomizer sfx basic -t4 -o $(BUILDFOLDER)/packed/rt-cbm-p4.prg $(BUILDFOLDER)/rt-cbm-p4.prg

# Generate version include
$(SRCFOLDER)/version.asm: $(SRCFOLDER)/version.txt | $(SRCFOLDER)
	echo !text '"'$(VERSION)'"' > $(SRCFOLDER)/version.asm

# Generate rules dynamically
$(eval $(foreach variant,$(CBMVARIANTS),$(call make_target_acme,$(variant))))
$(eval $(foreach variant,$(MSXVARIANTS),$(call make_target_pasmo,$(variant))))

# Clean target
clean:
	rm -rf $(BUILDFOLDER)