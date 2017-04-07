#include "SheepMessageBuffer.h"
#include <cstring>

using namespace Sheep;

MessageBuffer::MessageBuffer(size_t bufferSize)
    : mBufferSize(bufferSize)
{
    mBuffer = new char[mBufferSize];
    Clear();
}

MessageBuffer::~MessageBuffer()
{
    delete[] mBuffer;
    mBuffer = nullptr;
}

size_t MessageBuffer::Size() const
{
    return mBufferSize;
}

char* MessageBuffer::Data() const
{
    return mBuffer;
}

void MessageBuffer::Clear()
{
    memset(mBuffer, 0, mBufferSize);
}

std::string MessageBuffer::ToString()
{
    return std::string(mBuffer);
}
