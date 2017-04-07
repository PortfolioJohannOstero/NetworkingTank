#include "SheepMessageParser.h"
#include <assert.h>
#include <sstream>

using namespace Sheep;

MessageParser::MessageParser()
{}

MessageParser::~MessageParser()
{
    mParsedString.clear();
}

void MessageParser::ParseString(const std::string& message, char seperator)
{
    std::istringstream ss(message);
    std::string temp = "";
    while(std::getline(ss, temp, seperator))
        mParsedString.push_back(temp);
}

bool MessageParser::IsEmpty() const
{
    return mParsedString.size() == 0;
}

std::string MessageParser::operator[](int index)
{
    assert(index < mParsedString.size());
    return mParsedString[index];
}

std::string MessageParser::Head()
{
    if(mParsedString.size() > 0)
        return mParsedString[0];

    return "";
}

float MessageParser::ToFloat(int index)
{
    assert(index < mParsedString.size());
    float value = 0;
    // tries to convert the string into a float. If it fails it simply return 0.0f
    try
    {
        std::string::size_type sz;
        value = std::stof(mParsedString[index], &sz);
    }
    catch(int e)
    {
        return 0.0f;
    }

    return value;
}
