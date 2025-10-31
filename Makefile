NAME = ft_turing
NAME_BYTE = $(NAME).byte

SRC =  main.ml

OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)

LIB = yojson

OCAMLFIND = ocamlfind
OCAMLC = $(OCAMLFIND) ocamlc
OCAMLOPT = $(OCAMLFIND) ocamlopt
OCAMLFLAGS = -package $(LIB) -linkpkg

RM = rm -f

all: $(NAME) $(NAME_BYTE)

$(NAME): $(OPTOBJS)
	$(OCAMLOPT) $(OCAMLFLAGS) -o $(NAME) $(OPTOBJS)

$(NAME_BYTE): $(OBJS)
	$(OCAMLC) $(OCAMLFLAGS) -o $(NAME_BYTE) $(OBJS)

c: clean
clean:
	$(RM) *.cm[iox] *.o

f: fclean
fc: fclean
fclean: clean
	$(RM) $(NAME) $(NAME_BYTE)

re: fclean all

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

install:
	@if ! command -v ocamlfind &> /dev/null; then \
		opam install --yes ocamlfind; \
	fi
	@if ! opam list --installed $(LIB) > /dev/null 2>&1; then \
		opam install --yes $(LIB); \
	fi

.PHONY: all clean fclean