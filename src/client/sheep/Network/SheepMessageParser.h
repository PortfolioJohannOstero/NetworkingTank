#ifndef SHEEP_MESSAGEPARSER_H
#define SHEEP_MESSAGEPARSER_H

/* +================================+
 *  Name: MessageParser
 *  Author: Johann Ostero (P4220933)
 *  Type: Object
 *  --
 *  This class handles string parsing.
 *  The user needs to provide a string and any char seperator.
        The seperator will be used as the splitting point in the string.
 *  The string gets parsed into an std::vector of strings.
        These can be accessed using the [] operators or Head().
        It also supports string to other data type convertion
 * +================================+
 */

#include <vector>
#include <string>

namespace Sheep
{
    class MessageParser
    {
        public:
            MessageParser();
            ~MessageParser();

            void ParseString(const std::string& message, char seperator);

            bool IsEmpty() const;
            std::string operator[](int index);
            std::string Head();

            float ToFloat(int index);

        private:
            std::vector<std::string> mParsedString;
    };
}



#endif // SHEEP_MESSAGEPARSER_H
