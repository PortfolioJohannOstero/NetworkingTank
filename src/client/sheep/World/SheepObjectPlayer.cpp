#include "SheepObjectPlayer.h"

#include "../../Utils.hpp"
#include "SheepGameNetwork.h"
#include "SheepWorld.h"
#include "SheepAmmo.h"
#include "SheepView.h"

using namespace Sheep;

ObjectPlayer::ObjectPlayer(float speed, const Ammo& ammo, const std::string& objectName, int spriteId, Tag objectTag)
    : Object(objectName, objectTag, spriteId), mSpeed(speed)
{
    // Sets the spawn point to be somewhere random within the window
    const sf::Vector2u windowSize = VIEW.window.getSize();
    sprite->setPosition(Random<unsigned>(0, windowSize.x),
                        Random<unsigned>(0, windowSize.y));

    // Sends a message to the server to tell everyone that THIS player is going to join the game
    GAME_NETWORK.Join(objectName, sprite->getPosition(), sprite->getRotation());

    mPrevPos = sprite->getPosition();
    mPrevRot = sprite->getRotation();

    mRegular_ammo = new Ammo(ammo.GetMaxAmmo(), ammo.GetFireRate());
}

ObjectPlayer::~ObjectPlayer()
{
    delete mRegular_ammo;
    mRegular_ammo = nullptr;
}

void ObjectPlayer::Update()
{
    if(IsActive())
    {
        // PLAYER CONTROLS

            // Player Movement
        if(sf::Keyboard::isKeyPressed(sf::Keyboard::Left))
            sprite->rotate(-mSpeed);
        else if(sf::Keyboard::isKeyPressed(sf::Keyboard::Right))
            sprite->rotate(mSpeed);

        // Player rotation
        float currRot = degToRad(sprite->getRotation());
        if(sf::Keyboard::isKeyPressed(sf::Keyboard::Up))
            sprite->move(-mSpeed * sin(currRot), mSpeed * cos(currRot));
        else if(sf::Keyboard::isKeyPressed(sf::Keyboard::Down))
            sprite->move(mSpeed * sin(currRot), -mSpeed * cos(currRot));

        float newRot = sprite->getRotation();
        sf::Vector2f currPos = sprite->getPosition();

        // Shoot projectiles
        if(sf::Keyboard::isKeyPressed(sf::Keyboard::Space) && mRegular_ammo->CanShoot())
        {
            WORLD.ShootProjectile(*this); // tells the world to shoot a bullet, using THIS players collision tags

            // Tells the server to tell everyone that THIS player is shooting a projectile
            GAME_NETWORK.ShootProjectile(GetName(), currPos, newRot);
        }

        // Send updates to server
        if(mPrevPos != currPos)
        {
            mPrevPos = currPos;

            // Tells the server to tell everyone that THIS player has changed position
            GAME_NETWORK.UpdatePosition(GetName(), currPos);
        }

        if(mPrevRot != newRot)
        {
            mPrevRot = newRot;
            // Tells the server to tell everyone that THIS player has changed rotation
            GAME_NETWORK.UpdateRotation(GetName(), newRot);
        }
    }
    else
    {
        // If the player is inactive, pressing space will reactivate the player.
        if(sf::Keyboard::isKeyPressed(sf::Keyboard::Space))
        {
            // Tells the server to tell everyone that THIS player is respawning (reactivating)
            GAME_NETWORK.Respawn(GetName());
            SetActive(true);
        }
    }
}
