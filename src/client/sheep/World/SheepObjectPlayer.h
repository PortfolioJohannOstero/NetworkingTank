#ifndef SHEEP_OBJECTPLAYER_H
#define SHEEP_OBJECTPLAYER_H

/* +================================+
 *  Name: ObjectPlayer
 *  Author: Johann Ostero (P4220933)
 *  Type: Object
 *  --
 *  This is a subscriber to the Object class.
 *  This class is any given client that is playing the game.
 *  The inputs on this class will be treated as an ObjectEnemy on everyone elses machines on the server
 *  This class uses the Update() to move/rotate and shoot
 * +================================+
 */


#include "SheepObject.h"

namespace Sheep
{
    class Ammo;
    class ObjectPlayer : public Object
    {
    public:
        ObjectPlayer(float speed, const Ammo& ammo, const std::string& objectName = "Object", int spriteId = 0, Tag objectTag = Tag::PLAYER);
        virtual ~ObjectPlayer();

        void Update() override final;

    private:
        float mSpeed;
        Ammo* mRegular_ammo = nullptr;

        float mPrevRot;
        sf::Vector2f mPrevPos;
    };
}



#endif // SHEEP_OBJECTPLAYER_H
