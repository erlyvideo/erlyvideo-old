dist_erl_DATA =	amf.erl	amf0.erl amf3.erl amf_AbstractMessage.erl \
	amf_AsyncMessage.erl amf_CommandMessage.erl amf_AcknowledgeMessage.erl
erldir = $(ERLANG_INSTALL_LIB_DIR_amf)/src
dist_hrl_DATA = amf.hrl
hrldir = $(ERLANG_INSTALL_LIB_DIR_amf)/include
beam_DATA = $(dist_erl_DATA:.erl=.beam) amf.app
beamdir = $(ERLANG_INSTALL_LIB_DIR_amf)/ebin
html_DATA = $(dist_erl_DATA:.erl=.html) erlang.png edoc-info stylesheet.css \
	index.html overview-summary.html modules-frame.html packages-frame.html
htmldir = $(ERLANG_INSTALL_LIB_DIR_amf)/doc
dist_noinst_DATA = overview.edoc.in
CLEANFILES = $(beam_DATA) $(html_DATA) overview.edoc
ERLCFLAGS += -W

$(beam_DATA): $(dist_hrl_DATA)

.erl.beam:
	@echo "Compiling $<..."
	@$(ERLC) $(ERLCFLAGS) $<

$(html_DATA): $(dist_erl_DATA) overview.edoc
	@echo "Generating documentation..."
	@$(ERL) -noshell -run edoc_run application amf '"."' '[{dir,"."}]'

check:
	@echo "Running unit tests..."
	@$(ERL) -eval "eunit:test({application,amf})" -noshell -s init stop
