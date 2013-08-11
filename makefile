SRC_DIR := src
OBJ_DIR := obj
BIN_DIR := bin
LIB_DIR := $(HOME)/.opam/system/lib
CFLAGS := -I $(OBJ_DIR)
OCAML = ocamlfind ocamlopt -package netclient -package yojson -package yojson.biniou

all: $(OBJ_DIR) $(BIN_DIR) $(BIN_DIR)/solver

$(BIN_DIR)/solver: $(OBJ_DIR)/bv.cmx $(OBJ_DIR)/contest_api.cmx $(OBJ_DIR)/client.cmx 
	$(OCAML) $(CFLAGS) -o $@ -linkpkg $^ 

$(OBJ_DIR)/client.cmx: $(OBJ_DIR)/bv.cmx $(OBJ_DIR)/contest_api.cmx $(SRC_DIR)/client.ml
	$(OCAML) -c $(CFLAGS) -o $@ $(SRC_DIR)/client.ml

$(OBJ_DIR)/bv.cmx: $(SRC_DIR)/bv.ml
	$(OCAML) -c $(CFLAGS) -o $@ $^

$(OBJ_DIR)/contest_api.cmx: $(OBJ_DIR)/bv.cmx $(SRC_DIR)/contest_api.ml
	$(OCAML) -c $(CFLAGS) -o $@ $(SRC_DIR)/contest_api.ml

$(OBJ_DIR):
	mkdir $(OBJ_DIR)

$(BIN_DIR):
	mkdir $(BIN_DIR)

clean:
	rm -rf $(OBJ_DIR)
	rm -rf $(BIN_DIR)

