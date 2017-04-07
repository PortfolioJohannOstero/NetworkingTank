#include <SFML/Graphics.hpp>
#include <iostream>
#include <ctime>
#include <sstream>
#include <string>
#include <vector>
#include <thread>


#include "sheep/Network/SheepNetTCP.h"
#include "sheep/Network/SheepNetUDP.h"
#include "sheep/Network/SheepNetUDPBroadcast.h"

#include "sheep/World/SheepWorld.h"
#include "sheep/Render/SheepView.h"
#include "sheep/Network/SheepGameNetwork.h"
#include "sheep/World/SheepObjectPlayer.h"
#include "sheep/World/SheepObjectProjectile.h"
#include "sheep/World/SheepAmmo.h"

#include "Utils.hpp"

#define BCAST_PORT "6000"
#define BCAST "152.105.67.255"

int main()
{
    // Broadcasting
    Sheep::NetUDPBroadcast udpBroadcast;
    udpBroadcast.Open(std::string(BCAST), std::string(BCAST_PORT));
    const std::string port = udpBroadcast.Receive();
    const std::string ipServer = udpBroadcast.GetServerIP();


    /* +=== Init Game Network ===+ */
    GAME_NETWORK.Create();
    if(!GAME_NETWORK.Initialize(ipServer, port, ':'))
    {
        std::cerr << "Failed to create SheepGameNetwork" << std::endl;
        return -1;
    }

    // Register client
    std::string name;
    do
    {
        std::cout << "name: ";
        std::cin >> name;
    } while(!GAME_NETWORK.Register(name));

    std::cout << "Name accepted!" << std::endl;


    /* +=== Init World ===+ */
    WORLD.Create();
    if(!WORLD.Initialise(800, 600, "Destructive Networking!"))
    {
        std::cerr << "Failed to Create SheepWorld/SheepView" << std::endl;
        return -1;
    }

    // Create textures and fonts
    int tankTex = 0;
    if(!VIEW.AddTexture("resources/Tank.gif", tankTex))
        std::cerr << "failed to load in sprite" << std::endl;

    int groundTex = 0;
    if(!VIEW.AddTexture("resources/ground.jpg", groundTex))
        std::cerr << "failed to load in sprite" << std::endl;

    int bulletTex = 0;
    if(!VIEW.AddTexture("resources/rocket.gif", bulletTex))
        std::cerr << "failed to load in sprite" << std::endl;

    int tankFont = 0;
    if(!VIEW.AddFont("resources/Copycat 759.ttf", tankFont))
        std::cerr << "failed to load in font" << std::endl;


    WORLD.SetBackground(groundTex);


    // Player setup
    Sheep::Ammo regularAmmo(300, 0.2f);
    Sheep::ObjectPlayer player(5.0f, regularAmmo, name, tankTex);
    player.AddCollisionTag(Sheep::Tag::ENEMY);
    WORLD.AddObject(&player);


    // Projectile setup
    Sheep::ObjectProjectile projectile(0.5f, "bullet", bulletTex);
    WORLD.AddProjectile(&projectile, 30);


    // Execute world (constant loop)
    WORLD.Run(60);

    return 0;
}


