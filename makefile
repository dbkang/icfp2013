SRC_DIR := src
OBJ_DIR := obj
BIN_DIR := bin
LIB_DIR := $(HOME)/.opam/system/lib
CFLAGS := -I $(OBJ_DIR) -I $(LIB_DIR)/yojson
OCAML = ocamlopt

all: $(OBJ_DIR) $(BIN_DIR) $(BIN_DIR)/solver

$(BIN_DIR)/solver: $(OBJ_DIR)/bv.cmx $(OBJ_DIR)/client.cmx 
	$(OCAML) $(CFLAGS) -o $@ $(LIB_DIR)/easy-format/easy_format.cmx $(LIB_DIR)/biniou/biniou.cmxa $(LIB_DIR)/yojson/yojson.cmx $^ 

$(OBJ_DIR)/client.cmx: $(OBJ_DIR)/bv.cmx $(SRC_DIR)/client.ml
	$(OCAML) -c $(CFLAGS) -o $@ $(SRC_DIR)/client.ml

$(OBJ_DIR)/bv.cmx: $(SRC_DIR)/bv.ml
	$(OCAML) -c $(CFLAGS) -o $@ $^

$(OBJ_DIR):
	mkdir $(OBJ_DIR)

$(BIN_DIR):
	mkdir $(BIN_DIR)

clean:
	rm -rf $(OBJ_DIR)
	rm -rf $(BIN_DIR)

