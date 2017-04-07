#include "SheepNetUDPBroadcast.h"

#include <iostream>

using namespace Sheep;

NetUDPBroadcast::NetUDPBroadcast()
{
}

NetUDPBroadcast::~NetUDPBroadcast()
{}

bool NetUDPBroadcast::InitSocket()
{
    // Tries to init the socket, using the parent method
    if(!NetUDP::InitSocket())
        return false;

    // Setup Broadcasting
    int bcastEnabled = 1;
    int result = setsockopt(mSocket, SOL_SOCKET, SO_BROADCAST, &bcastEnabled, sizeof(bcastEnabled));
    if(result != 0)
    {
        perror("setsockopt");
        return false;
    }

    return true;
}

bool NetUDPBroadcast::Open(const std::string& hostname, const std::string& broadcastPort)
{
    // Collects the IP details
    struct addrinfo hints, *result;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_DGRAM;

    int rv = getaddrinfo(hostname.c_str(), broadcastPort.c_str(), &hints, &result);
    if (rv != 0)
    {
        perror("getaddrinfo");
        return false;
    }


    // Updates the Socket
    mSocket = socket(result->ai_family, result->ai_socktype, result->ai_protocol);
    if(bind(mSocket, result->ai_addr, result->ai_addrlen) < 0)
    {
        perror("ERROR on binding");
        return false;
    }

    return true;
}

std::string NetUDPBroadcast::Receive()
{
    mRecvBuffer->Clear();

    // Receive the message itself
    struct sockaddr_in adr;
    unsigned int s = sizeof(adr);
    memset(&adr, 0, s);
    int n = recvfrom(mSocket,mRecvBuffer->Data(),mRecvBuffer->Size(),0, (struct sockaddr *) &adr,&s);
    if (n < 0)
    {
        perror("recv");
        return "";
    }

    // Gets the sender IP
    char addr[INET_ADDRSTRLEN];
    memset(addr, 0, INET_ADDRSTRLEN);
    const char *r_ntop = inet_ntop(AF_INET, &(adr.sin_addr), addr, INET_ADDRSTRLEN);
    if (r_ntop == NULL)
    {
        perror("inet_ntop");
        return "";
    }

    mServerIP = std::string(addr);
    return mRecvBuffer->ToString();
}


std::string NetUDPBroadcast::GetServerIP() const
{
    return mServerIP;
}
