SRC_DIR := src
OBJ_DIR := obj
BIN_DIR := bin
CFLAGS := -c -I $(OBJ_DIR) -I $(HOME)/.opam/system/lib/yojson
OCAML = ocamlopt

all: $(OBJ_DIR) $(BIN_DIR) $(BIN_DIR)/solver

$(BIN_DIR)/solver: $(OBJ_DIR)/bv.cmx $(OBJ_DIR)/client.cmx
	$(OCAML) -o $@ $^ 

$(OBJ_DIR)/client.cmx: $(OBJ_DIR)/bv.cmx $(SRC_DIR)/client.ml
	$(OCAML) $(CFLAGS) -o $@ $(SRC_DIR)/client.ml

$(OBJ_DIR)/bv.cmx: $(SRC_DIR)/bv.ml
	$(OCAML) $(CFLAGS) -o $@ $^

$(OBJ_DIR):
	mkdir $(OBJ_DIR)

$(BIN_DIR):
	mkdir $(BIN_DIR)

clean:
	rm -rf $(OBJ_DIR)
	rm -rf $(BIN_DIR)

