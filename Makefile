INCLUDES	=	-I./include

WARNINGS	=	-Wall -Wextra -Werror=implicit-fallthrough=0

CXXFLAGS	=	-std=c++20 $(WARNINGS) -g3 $(INCLUDES)

SRC			=	src/Main.cpp		\
				src/GladosGenerator.cpp

OBJ			=	$(patsubst src/%,obj/%,$(SRC:.cpp=.o))

NAME		=	GladosGenerator

.PHONY: clean fclean re 

all: $(NAME)

obj/%.o: src/%.cpp
	g++ -c $(CXXFLAGS) $< -o $@

$(NAME): $(OBJ)
	g++ -o $(NAME) $(OBJ)

clean:
	rm -f vgcore*
	rm -f $(OBJ)

fclean: clean
	rm -f $(NAME)
	rm -Rf Output
	mkdir Output
	chmod 777 Output
	touch Output/.gitkeep

re: fclean all
