#include "SheepNetTCP.h"

using namespace Sheep;

NetTCP::NetTCP() {}

NetTCP::~NetTCP()
{
    if(mSocket != -1)
        CloseSocket();
}

bool NetTCP::InitSocket()
{
    mSocket = socket(AF_INET, SOCK_STREAM, 0);
    return mSocket >= 0; // if it is less than 0 then it is considered an error
}

bool NetTCP::Connect(const std::string& hostname, const std::string&  serverPort)
{
   const sockaddr_in* result = SetupAddress(hostname, serverPort, SOCK_STREAM); // <-- Gets the server ip

   bool connected = connect(mSocket, (struct sockaddr*)result, sizeof(*result)) != -1;
   return connected;
}


int NetTCP::Send(const std::string& message)
{
    int s = send(mSocket, message.c_str(), message.length(), 0);
    return s;
}

std::string NetTCP::Receive()
{
    mRecvBuffer->Clear();
    recv(mSocket, mRecvBuffer->Data(), mRecvBuffer->Size(), 0);

    return mRecvBuffer->ToString();
}




