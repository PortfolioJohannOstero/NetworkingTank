#include "SheepObjectProjectile.h"
#include "SheepView.h"
#include "../../Utils.hpp"

#include "SheepObjectEnemy.h"
#include "SheepObjectPlayer.h"

using namespace Sheep;

ObjectProjectile::ObjectProjectile(float speed, const std::string& objectName, int spriteId, Tag objectTag)
    : mSpeed(speed), Object(objectName, objectTag, spriteId)
{
    SetActive(false);
}

ObjectProjectile::~ObjectProjectile()
{
    //dtor
}

void ObjectProjectile::Update()
{
    if(IsActive())
    {
        // constantly travels in a given direction
        const float currRot = degToRad(sprite->getRotation());
        sprite->move(-mSpeed * sin(currRot), mSpeed * cos(currRot));

        // if outside the window
        const sf::Vector2f& projPos = sprite->getPosition();
        const sf::Vector2u& windowSize = VIEW.window.getSize();

        SetActive(projPos.x > 0 && projPos.x < windowSize.x &&
                  projPos.y > 0 && projPos.y < windowSize.y);
    }
}


void ObjectProjectile::OnCollisionEnter(Object* otherObject)
{
    SetActive(false);

    // check for a collision with the ENEMY or PLAYER
        // However will only affect one or the other depending on who shot it, due to the use of Tag Collision Containers
    if(otherObject->GetTag() == Tag::ENEMY || otherObject->GetTag() == Tag::PLAYER)
        otherObject->SetActive(false);
}
