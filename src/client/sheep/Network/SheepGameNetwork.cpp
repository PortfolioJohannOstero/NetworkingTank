#include <iostream>
#include <sstream>

#include "SheepGameNetwork.h"
#include "SheepNetwork.h"
#include "SheepNetTCP.h"
#include "SheepNetUDP.h"
#include "SheepMessageParser.h"

#include "../../Utils.hpp"
#include "SheepWorld.h"
#include "SheepObjectEnemy.h"

using namespace Sheep;

/* +=== Singleton Construction ===+ */
GameNetwork* GameNetwork::INSTANCE = nullptr;

void GameNetwork::Create()
{
    if(!INSTANCE)
        INSTANCE = new GameNetwork();
}

void GameNetwork::Destroy()
{
    if(INSTANCE)
    {
        delete INSTANCE;
        INSTANCE = nullptr;
    }
}

bool GameNetwork::Initialize(const std::string& serverIp, const std::string& serverPort, char msgSeperator)
{
    mMessageSeperator = msgSeperator;

    /* +== Set up UDP/TCP ==+ */
    mTCP = new NetTCP();
    mUDP = new NetUDP();

    mTCP->InitSocket();
    mUDP->InitSocket();

    mTCP->Connect(serverIp, serverPort);
    mUDP->Open(serverIp, serverPort);

    return true;
}

GameNetwork::~GameNetwork()
{
    delete mTCP;
    mTCP = nullptr;

    delete mUDP;
    mUDP = nullptr;

    delete mThread_tcp;
    mThread_tcp = nullptr;
    delete mThread_udp;
    mThread_udp = nullptr;
}

// +=== close
void GameNetwork::Close()
{
    mTCP->CloseSocket();
    mUDP->CloseSocket();

    mThread_tcp->detach();
    mThread_udp->detach();
}

bool GameNetwork::IsRegistered() const
{
    return mIsRegistered;
}

/* +=== Server Messages ===+ */
bool GameNetwork::Register(const std::string& userName)
{
    std::stringstream ss;
    ss << "reg:" << userName << ":" << mUDP->GetIP() << ":" << mUDP->GetPort() << ":0:0";
    mTCP->Send(ss.str());

    if(mTCP->Receive() != "reg_success")
        return false;

    /* +== Set up Threads ==+ */
    mThread_tcp = new std::thread(&GameNetwork::thread_receive, this, std::ref(*mTCP));
    mThread_udp = new std::thread(&GameNetwork::thread_receive, this, std::ref(*mUDP));

    return true;
}

void GameNetwork::Unregister(const std::string& userName)
{
    mTCP->Send("unreg:" + userName);
}

void GameNetwork::Join(const std::string& userName, const sf::Vector2f& pos, float angle)
{
    std::stringstream ss;
    ss << "join:" << userName << ":" << pos.x << ":" << pos.y << ":" << angle;
    mTCP->Send(ss.str());
}

void GameNetwork::UpdatePosition(const std::string& userName, const sf::Vector2f& pos)
{
    std::stringstream ss;
    ss << "set_pos:" << userName << ":" << pos.x << ":" << pos.y;
    mUDP->Send(ss.str());
}

void GameNetwork::UpdateRotation(const std::string& userName, float angle)
{
    std::stringstream ss;
    ss << "set_rot:" << userName << ":" << angle;
    mUDP->Send(ss.str());
}

void GameNetwork::ShootProjectile(const std::string& userName, const sf::Vector2f& pos, float angle)
{
    std::stringstream ss;
    ss << "shoot:" << userName << ":" << pos.x << ":" << pos.y << ":" << angle;
    mUDP->Send(ss.str());
}

void GameNetwork::Respawn(const std::string& name)
{
    mTCP->Send("respawn:" + name);
}


/* +==== Receive Thread ====+ */
void GameNetwork::thread_receive(Network& netRecv)
{
    while(true)
    {
        MessageParser parser;
        parser.ParseString(netRecv.Receive(), mMessageSeperator);

        // respawned:name
        if(parser.Head() == "respawned")
            WORLD.RespawnObject(parser[1]);

        // get_all_clients:name:posX:posY:angle
        else if(parser.Head() == "join_client")
        {
            // Join Cliend creates a new object that is then stored in the World
            std::cout << "Found Player: " << parser[1] << std::endl;
            ObjectEnemy* newPlayer = new ObjectEnemy(0.1f, parser[1]);
            newPlayer->sprite->setPosition(sf::Vector2f(parser.ToFloat(2), parser.ToFloat(3)));
            newPlayer->sprite->setRotation(parser.ToFloat(4));
            newPlayer->AddCollisionTag(Tag::PLAYER);

            newPlayer->SetActive(true);

            mIsJoining = true;
            WORLD.AddObject(newPlayer);
            mIsJoining = false;
        }
        // remove_client:name
        else if(parser.Head() == "removed_client")
        {
            std::cout << "Connection lost with client: " << parser[1] << std::endl;
            WORLD.RemoveObject(parser[1]); // <-- deletes the object
        }
        // update_pos:name:posX:posY
        else if(!mIsJoining && parser.Head() == "update_pos")
            WORLD.UpdateObjectPosition(parser[1], sf::Vector2f(parser.ToFloat(2), parser.ToFloat(3)));
        // update_rot:name:angle
        else if(!mIsJoining && parser.Head() == "update_rot")
            WORLD.UpdateObjectRotation(parser[1], radToDeg(parser.ToFloat(2)));

        // shot:name:posX:posY:angle
        else if(parser.Head() == "shot")
            WORLD.ShootProjectile(parser[1], sf::Vector2f(parser.ToFloat(2), parser.ToFloat(3)), parser.ToFloat(4));
    }
}
