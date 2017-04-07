#ifndef SHEEP_AMMO_H
#define SHEEP_AMMO_H

/* +================================+
 *  Name: Ammo
 *  Author: Johann Ostero (P4220933)
 *  Type: Object
 *  --
 *  This class is a basic Ammo simulator that requires the max amount of ammo and how fast it will shoot.
 *  It does not shoot, but only returns a true or false; can shot or cannot shot.
 * +================================+
 */

namespace Sheep
{
    class Ammo
    {
        public:
            // firerate: 0.0f - 1.0f
            Ammo(unsigned int maxAmmo, float fireRate);
            virtual ~Ammo();

            // Will increment the mCurrTime when called.
            // Will return true and decrement the mCurrAmmo if the mCurrTime >= 1.0f
            bool CanShoot();
            void AddAmmo(unsigned int ammo);

            unsigned int GetMaxAmmo() const;
            float GetFireRate() const;

        private:
            unsigned int mMaxAmmo;
            float mFireRate;

            unsigned int mCurrAmmo;
            float mCurrTime;
    };
}


#endif // SHEEP_AMMO_H
