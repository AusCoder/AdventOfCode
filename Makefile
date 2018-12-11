TARGET = day8
LIBS =
CC = clang++
CXXFLAGS = -g -Wall -std=c++14
HEADERS =

default: $(TARGET)

$(TARGET).o: $(TARGET).cpp $(HEADERS)
	$(CC) $(CXXFLAGS) -c $(TARGET).cpp -o $(TARGET).o

$(TARGET): $(TARGET).o
	$(CC) $(TARGET).o -o $(TARGET)

clean:
	-rm -f $(TARGET).o
	-rm -f $(TARGET)
