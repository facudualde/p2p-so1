# Makefile con verificación de contenido

REBAR3 := rebar3
REBAR_CONFIG := rebar.config
TEMP_CONFIG := .rebar.config.expected

# Contenido esperado del rebar.config
define REBAR_CONFIG_CONTENT
{deps, [
  {jsx, "2.11.0"}
]}.
endef
export REBAR_CONFIG_CONTENT

all: verify-rebar-config compile

# Verifica y actualiza rebar.config si es necesario
verify-rebar-config:
	@echo "$$REBAR_CONFIG_CONTENT" > $(TEMP_CONFIG)
	@cmp -s $(TEMP_CONFIG) $(REBAR_CONFIG) || cp $(TEMP_CONFIG) $(REBAR_CONFIG)
	@rm -f $(TEMP_CONFIG)
compile: verify-rebar-config
	$(REBAR3) compile


run: compile
	$(REBAR3) shell

deps: verify-rebar-config
	$(REBAR3) get-deps

.PHONY: all verify-rebar-config compile run deps