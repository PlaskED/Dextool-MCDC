#include "foo.hpp"
#include <cmath>
//#include <iostream>
namespace Foo {
  // Set functions
  void Ray3::SetRay(Vec3<double> o, Vec3<double> d)
  {
    origin = o;
    direction = d;
  }
  
  // Default constructor (set to all zeros)
  Ray3::Ray3()
  {
    Vec3<double> o, d;
    SetRay(o, d);
  }
  // Constructor with origin and direction setter
  Ray3::Ray3(Vec3<double> o, Vec3<double> d)
  {
    SetRay(o, d);
  }
  
  Bar::Bar(Vec3<double> c, double r, double h)
  {
    centroid = c;
    radius = r;
    height = h;
  }
  /*double Bar::CalcVolume()
  {
    volume = M_PI*pow(radius,2.0)*height;
    return volume;
    }*/

  double Bar::RayPathlength(Ray3 ray)
  {
    //double solution[2];
    double L = ray.direction.Magnitude();
    if (L > 2) {
      return L;
    }
    // Calculate coefficients for the quadratic equation
    //double q_a = pow(ray.direction.x,2) + pow(ray.direction.y,2);
    //double q_b = 2*ray.direction.x*(ray.origin.x-centroid.x) +
    //  2*(ray.direction.y*(ray.origin.y - centroid.y));
    //double q_c = pow((ray.origin.x-centroid.x),2) + pow((ray.origin.y-centroid.y),2) - pow(radius,2);
    //double q_check = pow(q_b,2) - 4*q_a*q_c;
    // No solution
    //if(q_check < 0.0) return 0.0;
    // One or two solutions
    // else
    //  {
    //	solution[0] = (-q_b + sqrt(q_check)) / (2*q_a);
    //	solution[1] = (-q_b - sqrt(q_check)) / (2*q_a);
    //	double d_center = sqrt(pow(ray.origin.x-centroid.x,2.0)+pow(ray.origin.y-centroid.y,2.0));
    //	if(d_center < radius)
    //	  {
    //	    double positive_value = std::max(solution[0], solution[1]);
    //	    return (L * positive_value);
    //	  }
    //	else return (L * fabs(solution[0]-solution[1]));
    //  }
    return 0.0;
  }
}
