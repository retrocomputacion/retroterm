
BUILDFOLDER:=build
SRCFOLDER:=source

VARIANTS=u sl ulti 232

MVARIANTS=232 56k 38k

DEFINES_FOR_u=-D_HARDTYPE_=56
DEFINES_FOR_sl=-D_HARDTYPE_=38
DEFINES_FOR_ulti=-D_HARDTYPE_=1541
DEFINES_FOR_232=-D_HARDTYPE_=232


# Read build version from source/version.txt
VERSION:=$(file < $(SRCFOLDER)/version.txt)



define make_target_acme =
$$(BUILDFOLDER)/rt-$(1).prg: $(SRCFOLDER)/retroterm_univ.asm $(SRCFOLDER)/version.asm
	acme $$(DEFINES_FOR_$(1)) -D_MAKE_=1 -I $(SRCFOLDER) -f cbm -o $$@ $$<

endef

all: $(SRCFOLDER)/version.asm $(foreach variant,$(VARIANTS),$(BUILDFOLDER)/rt-$(variant).prg ) plus4 msx232 msx56k msx38k
comm: $(SRCFOLDER)/version.asm $(foreach variant,$(VARIANTS),$(BUILDFOLDER)/rt-$(variant).prg ) plus4
msx: $(SRCFOLDER)/version.asm $(foreach variant,$(MVARIANTS),$(BUILDFOLDER)/rt$(variant).com )


swiftlink: $(BUILDFOLDER)/rt-sl.prg

ultimate: $(BUILDFOLDER)/rt-ulti.prg

turbo232: $(BUILDFOLDER)/rt-232.prg

userport: $(BUILDFOLDER)/rt-u.prg

plus4: $(BUILDFOLDER)/rt-p4.prg

msx232: $(BUILDFOLDER)/rt232.com

msx56k: $(BUILDFOLDER)/rt56k.com

msx38k: $(BUILDFOLDER)/rt38k.com

$(BUILDFOLDER)/rt_p4_v$(VERSION).prg: $(SRCFOLDER)/retrotermp4.asm
	acme -f cbm -D_MAKE_=1 -I $(SRCFOLDER) -o $(BUILDFOLDER)/rt_p4_v$(VERSION).prg $(SRCFOLDER)/retrotermp4.asm

$(BUILDFOLDER)/rt232.com: $(SRCFOLDER)/retrotermm1.asm
	pasmo -E IFACE=0 -I $(SRCFOLDER) $(SRCFOLDER)/retrotermm1.asm $(BUILDFOLDER)/rt232.com $(SRCFOLDER)/rtm232.symbol
$(BUILDFOLDER)/rt56k.com: $(SRCFOLDER)/retrotermm1.asm
	pasmo -E IFACE=56 -I $(SRCFOLDER) $(SRCFOLDER)/retrotermm1.asm $(BUILDFOLDER)/rt56k.com $(SRCFOLDER)/rtm56k.symbol
$(BUILDFOLDER)/rt38k.com: $(SRCFOLDER)/retrotermm1.asm
	pasmo -E IFACE=38 -I $(SRCFOLDER) $(SRCFOLDER)/retrotermm1.asm $(BUILDFOLDER)/rt38k.com $(SRCFOLDER)/rtm38k.symbol

$(SRCFOLDER)/version.asm: $(SRCFOLDER)/version.txt
# Generate version include source/version.asm
	echo !text '"'$(VERSION)'"' > $(SRCFOLDER)/version.asm

#Create rules
$(eval $(foreach variant,$(VARIANTS),$(call make_target_acme,$(variant))))

