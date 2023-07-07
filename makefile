
BUILDFOLDER:=build
SRCFOLDER:=source

VARIANTS=u sl ulti 232

DEFINES_FOR_u=-D_HARDTYPE_=56
DEFINES_FOR_sl=-D_HARDTYPE_=38
DEFINES_FOR_ulti=-D_HARDTYPE_=1541
DEFINES_FOR_232=-D_HARDTYPE_=232

# Read build version from source/version.txt
VERSION:=$(file < $(SRCFOLDER)/version.txt)



define make_target =
$$(BUILDFOLDER)/rt_$(1)_v$(VERSION).prg: $(SRCFOLDER)/retroterm_univ.asm $(SRCFOLDER)/version.asm
	acme $$(DEFINES_FOR_$(1)) -D_MAKE_=1 -I $(SRCFOLDER) -f cbm -o $$@ $$<

endef

all: $(SRCFOLDER)/version.asm $(foreach variant,$(VARIANTS),$(BUILDFOLDER)/rt_$(variant)_v$(VERSION).prg ) plus4

swiftlink: $(BUILDFOLDER)/rt_sl_v$(VERSION).prg

ultimate: $(BUILDFOLDER)/rt_ulti_v$(VERSION).prg

turbo232: $(BUILDFOLDER)/rt_232_v$(VERSION).prg

userport: $(BUILDFOLDER)/rt_u_v$(VERSION).prg

plus4: $(BUILDFOLDER)/rt_p4_v0.10.prg

$(BUILDFOLDER)/rt_p4_v0.10.prg: $(SRCFOLDER)/retrotermp4.asm
	acme -f cbm -D_MAKE_=1 -o $(BUILDFOLDER)/rt_p4_v0.10.prg $(SRCFOLDER)/retrotermp4.asm

$(SRCFOLDER)/version.asm: $(SRCFOLDER)/version.txt
# Generate version include source/version.asm
	echo !text '"'$(VERSION)'"' > $(SRCFOLDER)/version.asm

#Create rules
$(eval $(foreach variant,$(VARIANTS),$(call make_target,$(variant))))