#ifndef SHEEPMESSAGEBUFFER_H
#define SHEEPMESSAGEBUFFER_H

/* +================================+
 *  Name: MessageBuffer
 *  Author: Johann Ostero (P4220933)
 *  Type: Object
 *  --
 *  This class is a basic MessageBuffer of chars.
 *  It relies on a fixed size, specified in the constructor.
 *  It is a cheap copy of std::vector, in the sense that it allows the user
        to get the buffer size, get the char array and to clear it completely.
 *  Converting the char array to a string is also available.
 * +================================+
 */

#include <string>

namespace Sheep
{
    class MessageBuffer
    {
        public:
            MessageBuffer(size_t bufferSize);
            virtual ~MessageBuffer();

            size_t Size() const;
            char* Data() const; // <--- returns the char array
            void Clear(); // <--  Sets every value in the buffer to 0

            std::string ToString();

        private:
            char* mBuffer = nullptr;
            size_t mBufferSize;
    };
}


#endif // SHEEPMESSAGEBUFFER_H
