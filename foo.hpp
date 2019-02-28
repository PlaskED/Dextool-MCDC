#ifndef FOO_HPP
#define FOO_HPP
#include <cmath>
//#include <vector>
//#include <iostream>

namespace Foo {

  template <class T>
  class Vec3
  {
  public:
    // Default constructor (set to all zeros)
    Vec3();
    // Constructor with component setter
    Vec3(T ix, T iy, T iz);
    T x, y, z;

    //Math functions
    T Magnitude();
  };
  
  template <class T>
  Vec3<T>::Vec3()
  {
    x = y = z = 0;
  }

  
  template <class T>
  Vec3<T>::Vec3(T ix, T iy, T iz)
  {
    Set(ix, iy, iz);
  }

  template <class T>
  T Vec3<T>::Magnitude()
  {
    return sqrt(pow(x,2) + pow(y,2) + pow(z,2));
  }
  
  class Ray3
  {
  public:
    // Default constructor (set to all zeros)
    Ray3();
    // Constructor with origin and direction setter
    Ray3(Vec3<double> o, Vec3<double> d);
    // Ray origin & direction
    Vec3<double> origin, direction;
    // Set functions
    void SetRay(Vec3<double> o, Vec3<double> d);
  };
  
  class BarObject
  {
  public:
    //virtual double RayPathlength(Ray3 ray){ return 0.0; };
  protected:
    Vec3<double> centroid;
    //double volume;
  };
  
  class Bar : public BarObject
  {
  public:
    //Constructor + setter
    Bar(Vec3<double> c, double r, double h);
    //Getters
    //Vec3<double> GetCentroid(){ return centroid; }
    //double GetRadius(){ return radius; }
    //double GetHeight(){ return height; }
    //Calc functions
    //double CalcVolume();
    double RayPathlength(Ray3 ray);
  private:
    double radius;
    double height;
  };
}

#endif
