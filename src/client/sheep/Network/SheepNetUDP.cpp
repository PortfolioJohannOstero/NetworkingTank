#include "SheepNetUDP.h"
#include <iostream>

#include <string.h> /* For strncpy */
#include <sys/ioctl.h>
#include <net/if.h>


using namespace Sheep;

NetUDP::NetUDP()
{
}

NetUDP::~NetUDP()
{
    delete mHostSocketAddress;
    mHostSocketAddress = nullptr;
}

bool NetUDP::InitSocket()
{
    mSocket = socket(AF_INET, SOCK_DGRAM, 0);
    if (mSocket < 0) // if it is less than 0 then it is considered an error
    {
        perror("socket");
        return false;
    }

    return true;
}

bool NetUDP::Open(const std::string& hostname, const std::string&  serverPort)
{
    mHostSocketAddress = SetupAddress(hostname, serverPort, SOCK_DGRAM);

    // Gets the computers local IP
        // Using UDP
    struct ifreq ifr;
    ifr.ifr_addr.sa_family = AF_INET;

    strncpy(ifr.ifr_name, "eth0", IFNAMSIZ-1);
    ioctl(mSocket, SIOCGIFADDR, &ifr);

    // Collects the local ip information
    sockaddr_in local_address;
    socklen_t addrSize = sizeof(local_address);
    memset(&local_address, 0, sizeof(addrSize));

    local_address.sin_family = AF_INET;
    local_address.sin_addr = ((struct sockaddr_in*)&ifr.ifr_addr)->sin_addr;
    local_address.sin_port=htons(0); // <-- sets the port to be automatically picked

    // Updates the socket
    if(bind(mSocket,(struct sockaddr*) &local_address, sizeof(local_address)) < 0)
    {
        perror("bind failed");
        return false;
    }
    return true;
}


int NetUDP::Send(const std::string& message)
{
    int s = sendto(mSocket, message.c_str(), message.length(), 0,
                  (struct sockaddr*)mHostSocketAddress, sizeof(*mHostSocketAddress));
    return s;
}

std::string NetUDP::Receive()
{
    mRecvBuffer->Clear();

    sockaddr_in address;
    int addrSize = sizeof(address);

    int r = recvfrom(mSocket,mRecvBuffer->Data(), mRecvBuffer->Size() ,0, (struct sockaddr*) &address, (socklen_t*)&addrSize);
    if(r < 0)
    {
        perror("recv");
        return "";
    }

    return mRecvBuffer->ToString();
}

// Gets the local auto generated port
uint16_t NetUDP::GetPort() const
{
    sockaddr_in sockAddr;
    socklen_t sockAddrSize = sizeof(sockAddr);

    getsockname(mSocket, (sockaddr*)&sockAddr, &sockAddrSize);
    return htons(sockAddr.sin_port);
}

// Gets the local IP address
std::string NetUDP::GetIP() const
{
    sockaddr_in sockAddr;
    socklen_t sockAddrSize = sizeof(sockAddr);

    getsockname(mSocket, (sockaddr*)&sockAddr, &sockAddrSize);
    return std::string(inet_ntoa(sockAddr.sin_addr));
}
