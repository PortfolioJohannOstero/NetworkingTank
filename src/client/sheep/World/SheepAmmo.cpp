#include "SheepAmmo.h"

using namespace Sheep;

Ammo::Ammo(unsigned int maxAmmo, float fireRate)
    : mMaxAmmo(maxAmmo), mFireRate(fireRate), mCurrAmmo(maxAmmo), mCurrTime(0)
{
}

Ammo::~Ammo() {}

void Ammo::AddAmmo(unsigned int ammo)
{
    if(ammo > mMaxAmmo)
        ammo = mMaxAmmo;

    mCurrAmmo = ammo;
}

bool Ammo::CanShoot()
{
    // Increments the mCurrTime with the firerate for every call
    if(mCurrTime >= 1.0f && mCurrAmmo >= 0)
    {
        mCurrTime = 0.0f;
        //mCurrAmmo--; <-- infinite ammo
        return true;
    }

    mCurrTime += mFireRate;
    return false;
}


unsigned int Ammo::GetMaxAmmo() const
{
    return mMaxAmmo;
}

float Ammo::GetFireRate() const
{
    return mFireRate;
}
