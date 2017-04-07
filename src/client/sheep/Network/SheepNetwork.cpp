#include "SheepNetwork.h"

#include <iostream>
#include <unistd.h>

using namespace Sheep;

Network::Network()
{
    mRecvBuffer = new MessageBuffer(BUFFER_SIZE);
}

Network::~Network()
{
    delete mRecvBuffer;
    mRecvBuffer = nullptr;
}

struct sockaddr_in* Network::SetupAddress(const std::string& ip,
                                          const std::string& port,
                                          __socket_type protocolType)
{
    /* Collects the Server information based on the provided
        IP address,
        Port
        UDP/TCP protocol */
    struct addrinfo hints;
    struct addrinfo *result;

    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET;
    hints.ai_socktype = protocolType;


    int r = getaddrinfo(ip.c_str(), port.c_str(), &hints, &result);
    if (r != 0)
    {
        perror("getaddrinfo");
        return false;
    }

    return (struct sockaddr_in *)result->ai_addr;
}

void Network::CloseSocket()
{
    close(mSocket);
}

