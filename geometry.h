#include <algorithm>
#include <cmath>
#include <iostream>
#include <vector>

double eps = 1e-7;

double pi = acosl(-1);

bool is_equal_double(double a, double b)
{
    return std::abs(a - b) < eps;
}

struct Vector;

class Line;

struct Point {

    double x = 0, y = 0;

    Point() = default;

    Point(double _x, double _y) : x(_x), y(_y)
    {
    }

    explicit Point(const Vector& vec);

    bool operator==(const Point& other) const
    {
        return is_equal_double(x, other.x) && is_equal_double(y, other.y);
    }

    bool operator!=(const Point& other) const
    {
        return !(*this == other);
    }

    Point& rotate(double angle)
    { /// rad
        Point copy;
        copy.x = x * std::cos(angle) - y * std::sin(angle);
        copy.y = x * std::sin(angle) + y * std::cos(angle);
        *this = copy;
        return *this;
    }

    Point& rotate(Point center, double angle);

    Point& reflex(Point center);

    Point& reflex(Line axis);

    Point& scale(Point center, double coefficient);
};

std::ostream& operator<<(std::ostream& out, const Point& point)
{
    out << point.x << ", " << point.y << '\n';
    return out;
}

std::istream& operator>>(std::istream& in, Point& point)
{
    in >> point.x >> point.y;
    return in;
}

double dist(const Point& A, const Point& B)
{
    return std::sqrt((A.x - B.x) * (A.x - B.x) + (A.y - B.y) * (A.y - B.y));
}

struct Vector {

    double x = 0, y = 0;

    Vector() = default;

    Vector(double _x, double _y) : x(_x), y(_y)
    {
    }

    explicit Vector(const Point& point) : x(point.x), y(point.y)
    {
    }

    bool operator==(const Vector& other) const
    {
        return is_equal_double(x, other.x) && is_equal_double(y, other.y);
    }

    bool operator!=(const Vector& other) const
    {
        return !(*this == other);
    }

    Vector& operator+=(const Vector& other)
    {
        x += other.x;
        y += other.y;
        return *this;
    }

    Vector& operator-=(const Vector& other)
    {
        x -= other.x;
        y -= other.y;
        return *this;
    }

    double operator*(const Vector& other) const
    { // векторное
        return x * other.y - y * other.x;
    }

    double operator%(const Vector& other) const
    { // скалярное
        return x * other.x + y * other.y;
    }

    Vector operator+(const Vector& other) const
    {
        Vector res = *this;
        res += other;
        return res;
    }

    Vector operator-(const Vector& other) const
    {
        Vector res = *this;
        res -= other;
        return res;
    }

    Vector operator*=(double k)
    {
        x *= k;
        y *= k;
        return *this;
    }

    Vector operator*(double k) const
    {
        Vector res = *this;
        res *= k;
        return res;
    }

    Vector operator/=(double k)
    {
        x /= k;
        y /= k;
        return *this;
    }

    Vector operator/(double k) const
    {
        Vector res = *this;
        res /= k;
        return res;
    }

    double length() const
    {
        return std::sqrt(x * x + y * y);
    }

    Vector& rotate(double angle)
    { /// rad
        Vector copy;
        copy.x = x * std::cos(angle) - y * std::sin(angle);
        copy.y = x * std::sin(angle) + y * std::cos(angle);
        *this = copy;
        return *this;
    }

    Vector& rotate(Point center, double angle)
    { /// grad
        Vector direction = (*this - Vector(center)).rotate(angle / 180.0 * pi);
        *this = Vector(center) + direction;
        return *this;
    }

    Vector& reflex(Point center)
    {
        Vector direction = *this - Vector(center);
        *this = Vector(center) - direction;
        return *this;
    }

    Vector& reflex(Line axis);

    Vector& scale(Point center, double coefficient)
    {
        Vector direction = *this - Vector(center);
        *this = Vector(center) + direction * coefficient;
        return *this;
    }
};

double dist(const Vector& A, const Vector& B)
{
    return std::sqrt((A.x - B.x) * (A.x - B.x) + (A.y - B.y) * (A.y - B.y));
}

Point::Point(const Vector& vec) : x(vec.x), y(vec.y)
{
}

Point& Point::rotate(Point center, double angle)
{
    return *this = Point(Vector(*this).rotate(center, angle));
}

Point& Point::reflex(Point center)
{
    return *this = Point(Vector(*this).reflex(center));
}

Point& Point::scale(Point center, double coefficient)
{
    return *this = Point(Vector(*this).scale(center, coefficient));
}

class Line
{

  private:
    Vector A, B;

    double k, b;

    bool is_vertical;

  public:
    Line(const Vector& _A, const Vector& _B) : A(_A), B(_B)
    {
        if (is_equal_double(_A.x, _B.x)) {
            k = 0;
            b = 0;
            is_vertical = true;
            return;
        }
        is_vertical = false;
        k = (_B.y - _A.y) / (_B.x - _A.x);
        b = _A.y - _A.x * k;
    }

    Line(const Point& _A, const Point& _B) : A(Vector(_A)), B(Vector(_B))
    {
        if (is_equal_double(_A.x, _B.x)) {
            k = 0;
            b = 0;
            is_vertical = true;
            return;
        }
        is_vertical = false;
        k = (_B.y - _A.y) / (_B.x - _A.x);
        b = _A.y - _A.x * k;
    }

    Line(double _k, double _b)
        : A(0, _b), B(1.0, _k + _b), k(_k), b(_b), is_vertical(false)
    {
    } // y = kx + b

    Line(const Point& _A, double _k)
        : A(Vector(_A)), B(Vector(_A) + Vector(1.0, _k)), k(_k),
          b(_A.y - _A.x * k), is_vertical(false)
    {
    }

    bool operator==(const Line& other) const
    {
        if (is_vertical != other.is_vertical) {
            return false;
        }
        if (is_vertical) {
            return is_equal_double(A.x, other.A.x);
        }
        return is_equal_double(b, other.b) && is_equal_double(k, other.k);
    }

    bool operator!=(const Line& other) const
    {
        return !(*this == other);
    }

    Vector projection(Vector point) const
    {
        Vector normal = Vector(B - A).rotate(pi / 2.0);
        double square = (B - A) * (point - A);
        double length = square / dist(A, B);
        normal *= length / normal.length();
        return point - normal;
    }

    Vector intersection(const Line& other) const
    {
        double area_1 = (B - A) * (other.A - A);
        double area_2 = (other.B - A) * (B - A);
        double coefficient = (area_1) / (area_1 + area_2);
        return other.A + (other.B - other.A) * coefficient;
    }
};

Vector& Vector::reflex(Line axis)
{
    Vector projection = axis.projection(*this);
    Vector direction = projection - *this;
    *this += direction * 2.0;
    return *this;
}

Point& Point::reflex(Line axis)
{
    return *this = Point(Vector(*this).reflex(axis));
}

class Shape
{

  public:
    virtual double perimeter() const = 0;

    virtual double area() const = 0;

    virtual bool operator==(const Shape& another) const = 0;

    virtual bool operator!=(const Shape& another) const = 0;

    virtual bool isCongruentTo(const Shape& another) const = 0;

    virtual bool isSimilarTo(const Shape& another) const = 0;

    virtual bool containsPoint(Point point) const = 0;

    virtual void rotate(const Point& center, double angle) = 0;

    virtual void reflex(const Point& center) = 0;

    virtual void reflex(const Line& axis) = 0;

    virtual void scale(const Point& center, double coefficient) = 0;

    virtual ~Shape()
    {
    }
};

class Ellipse : public Shape
{

  protected:
    Vector A, B;

    double sum_distance;

    double a, b;

  public:
    ~Ellipse()
    {
    }

    Ellipse()
    {
    }

    Ellipse(const Point& _A, const Point& _B, double _sum_distance)
        : A(Vector(_A)), B(Vector(_B)), sum_distance(_sum_distance),
          a(_sum_distance / 2.0)
    {
        b = std::sqrt(a * a - dist(A, B) * dist(A, B) / 4.0);
    }

    Ellipse(const Vector& _A, const Vector& _B, double _sum_distance)
        : A(_A), B(_B), sum_distance(_sum_distance), a(_sum_distance / 2.0)
    {
        b = std::sqrt(a * a - dist(A, B) * dist(A, B) / 4.0);
    }

    std::pair<Point, Point> focuses() const
    {
        return std::make_pair(Point(A), Point(B));
    }

    double eccentricity() const
    {
        return dist(A, B) / sum_distance;
    }

    Point center() const
    {
        return Point((A + B) / 2.0);
    }

    std::pair<Line, Line> directrices() const
    {
        Vector abscissa = B - A;
        abscissa /= abscissa.length();
        abscissa *= a / eccentricity();
        Vector first = Vector(center()) + abscissa;
        Vector second = Vector(center()) - abscissa;
        abscissa.rotate(pi / 2.0);
        return std::make_pair(
            Line(first, first + abscissa), Line(second, second + abscissa));
    }

    double perimeter() const override
    {
        return pi * (3.0 * (a + b) - std::sqrt((3.0 * a + b) * (a + 3.0 * b)));
    }

    double area() const override
    {
        return pi * a * b;
    }

    bool containsPoint(Point point) const override
    {
        return dist(Vector(point), A) + dist(Vector(point), B) <=
               sum_distance + eps;
    }

    void rotate(const Point& center, double angle) override
    {
        A.rotate(center, angle);
        B.rotate(center, angle);
    }

    void reflex(const Point& center) override
    {
        A.reflex(center);
        B.reflex(center);
    }

    void reflex(const Line& axis) override
    {
        A.reflex(axis);
        B.reflex(axis);
    }

    void scale(const Point& center, double coefficient) override
    {
        A.scale(center, coefficient);
        B.scale(center, coefficient);
        a *= coefficient;
        b *= coefficient;
        sum_distance *= coefficient;
    }

    bool operator==(const Shape& another) const override
    {
        Shape* copy = const_cast<Shape*>(&another);
        Ellipse* ellipse = dynamic_cast<Ellipse*>(copy);
        if (!copy) {
            return false;
        }
        return A == ellipse->A && B == ellipse->B && a == ellipse->a;
    }

    bool operator!=(const Shape& another) const override
    {
        return !(*this == another);
    }

    bool isCongruentTo(const Shape& another) const override
    {
        Shape* copy = const_cast<Shape*>(&another);
        Ellipse* ellipse = dynamic_cast<Ellipse*>(copy);
        if (!copy) {
            return false;
        }
        if (!is_equal_double(a, ellipse->a)) {
            return false;
        }
        if (!is_equal_double(b, ellipse->b)) {
            return false;
        }
        return is_equal_double(dist(A, B), dist(ellipse->A, ellipse->B));
    }

    bool isSimilarTo(const Shape& another) const override
    {
        Shape* copy = const_cast<Shape*>(&another);
        Ellipse* ellipse = dynamic_cast<Ellipse*>(copy);
        if (!copy) {
            return false;
        }
        return is_equal_double(eccentricity(), ellipse->eccentricity());
    }
};

class Circle : public Ellipse
{

  public:
    ~Circle()
    {
    }

    Circle()
    {
    }

    Circle(Point center, double radius)
        : Ellipse(Vector(center), Vector(center), 2 * radius)
    {
    }

    Circle(Vector center, double radius) : Ellipse(center, center, 2 * radius)
    {
    }

    using Ellipse::Ellipse;

    double radius() const
    {
        return a;
    }

    double perimeter() const override
    {
        return 2.0 * pi * a;
    }

    double area() const override
    {
        return pi * a * a;
    }
};

class Polygon : public Shape
{

  protected:
    std::vector<Vector> vertices;

    void make_clockwise()
    {
        double area = 0;
        int n = vertices.size();
        for (size_t i = 0; i < vertices.size(); ++i) {
            area += (vertices[(i + 1) % n] - vertices[i]) *
                    (vertices[(i + 2) % n] - vertices[i]);
        }
        if (area < eps) {
            std::reverse(vertices.begin(), vertices.end());
        }
    }

  public:
    ~Polygon()
    {
    }

    Polygon()
    {
    }

    Polygon(std::vector<Point> _vertices)
    {
        vertices.resize(_vertices.size());
        for (size_t i = 0; i < vertices.size(); ++i) {
            vertices[i] = Vector(_vertices[i]);
        }
        make_clockwise();
    }

    Polygon(std::initializer_list<Point> _vertices)
    {
        vertices.resize(_vertices.size());
        std::vector<Point> __vertices = _vertices;
        for (size_t i = 0; i < vertices.size(); ++i) {
            vertices[i] = Vector(__vertices[i]);
        }
        make_clockwise();
    }

    int verticesCount() const
    {
        return vertices.size();
    }

    const std::vector<Point> getVertices() const
    {
        std::vector<Point> _vertices;
        for (auto i : vertices) {
            _vertices.push_back(Point(i));
        }
        const std::vector<Point> return_value = _vertices;
        return return_value;
    }

    bool isConvex() const
    {
        int n = verticesCount();
        std::vector<int> sign(n);
        int ind_not_zero = 0;
        for (int i = 0; i < n; ++i) {
            Vector v1 = vertices[(i + 1) % n] - vertices[i];
            Vector v2 = vertices[(i + 2) % n] - vertices[(i + 1) % n];
            if (is_equal_double(v1 * v2, 0)) {
                sign[i] = 0;
                continue;
            }
            ind_not_zero = i;
            if (v1 * v2 > 0) {
                sign[i] = 1;
            } else {
                sign[i] = -1;
            }
        }
        for (int i = 0; i < n; ++i) {
            if (sign[i] != 0 && sign[i] != sign[ind_not_zero])
                return false;
        }
        return true;
    }

    double perimeter() const override
    {
        double result = 0;
        int n = verticesCount();
        for (int i = 0; i < n; ++i) {
            result += dist(vertices[i], vertices[(i + 1) % n]);
        }
        return result;
    }

    double area() const override
    {
        double result = 0;
        int n = verticesCount();
        for (int i = 0; i < n - 2; ++i) {
            result += (vertices[i + 1] - vertices[0]) *
                      (vertices[i + 2] - vertices[0]);
        }
        return std::abs(result) / 2.0;
    }

    void rotate(const Point& center, double angle) override
    {
        for (size_t i = 0; i < vertices.size(); ++i) {
            vertices[i].rotate(center, angle);
        }
    }

    void reflex(const Point& center) override
    {
        for (size_t i = 0; i < vertices.size(); ++i) {
            vertices[i].reflex(center);
        }
    }

    void reflex(const Line& axis) override
    {
        for (size_t i = 0; i < vertices.size(); ++i) {
            vertices[i].reflex(axis);
        }
    }

    void scale(const Point& center, double coefficient) override
    {
        for (size_t i = 0; i < vertices.size(); ++i) {
            vertices[i].scale(center, coefficient);
        }
    }

    bool operator==(const Shape& another) const override
    {
        Shape* copy = const_cast<Shape*>(&another);
        Polygon* polygon = dynamic_cast<Polygon*>(copy);
        if (!copy) {
            return false;
        }
        if (verticesCount() != polygon->verticesCount()) {
            return false;
        }
        int n = verticesCount();
        for (int i = 0; i < n; ++i) {
            if (vertices[i] == polygon->vertices[0]) {
                for (int j = 0; j < n; ++j) {
                    if (vertices[(i + j) % n] != polygon->vertices[j]) {
                        return false;
                    }
                }
                return true;
            }
        }
        return false;
    }

    bool operator!=(const Shape& another) const override
    {
        return !(*this == another);
    }

  private:
    std::vector<double> get_lengths() const
    {
        int n = verticesCount();
        std::vector<double> lengths(n);
        for (int i = 0; i < n; ++i) {
            lengths[i] = (vertices[(i + 1) % n] - vertices[i]).length();
        }
        return lengths;
    }

    std::vector<double> get_angles() const
    {
        int n = verticesCount();
        std::vector<double> angles(n);
        for (int i = 0; i < n; ++i) {
            double sinx = (vertices[(i + 2) % n] - vertices[(i + 1) % n]) *
                          (vertices[i] - vertices[(i + 1) % n]) / 2.0;
            double cosx = (vertices[(i + 2) % n] - vertices[(i + 1) % n]) %
                          (vertices[i] - vertices[(i + 1) % n]);
            angles[i] = std::atan2(sinx, cosx);
        }
        return angles;
    }

  public:
    bool isCongruentTo(const Shape& another) const override
    {
        Shape* copy = const_cast<Shape*>(&another);
        Polygon* polygon = dynamic_cast<Polygon*>(copy);
        if (!copy) {
            return false;
        }
        if (verticesCount() != polygon->verticesCount()) {
            return false;
        }
        int n = verticesCount();
        std::vector<double> lengths_1 = get_lengths();
        std::vector<double> angles_1 = get_angles();
        std::vector<double> lengths_2 = polygon->get_lengths();
        std::vector<double> angles_2 = polygon->get_angles();
        if (!is_equal_double(perimeter(), another.perimeter()) ||
            !is_equal_double(area(), another.area())) {
            return false;
        }
        for (int i = 0; i < n; ++i) {
            bool f = true;
            for (int j = 0; j < n; ++j) {
                if (!is_equal_double(lengths_1[(i + j) % n], lengths_2[j]) ||
                    !is_equal_double(angles_1[(i + j) % n], angles_2[j])) {
                    f = false;
                    break;
                }
            }
            if (f) {
                return true;
            }
        }
        for (int i = 0; i < n; ++i) {
            bool f = true;
            for (int j = 0; j < n; ++j) {
                if (!is_equal_double(lengths_1[(i + j) % n], lengths_2[j]) ||
                    !is_equal_double(angles_1[(i + j) % n], -angles_2[j])) {
                    f = false;
                    break;
                }
            }
            if (f) {
                return true;
            }
        }
        return false;
    }

    bool isSimilarTo(const Shape& another) const override
    {
        Shape* copy = const_cast<Shape*>(&another);
        Polygon* polygon = dynamic_cast<Polygon*>(copy);
        if (!copy) {
            return false;
        }
        if (verticesCount() != polygon->verticesCount()) {
            return false;
        }
        int n = verticesCount();
        std::vector<double> lengths_1 = get_lengths();
        std::vector<double> angles_1 = get_angles();
        std::vector<double> lengths_2 = polygon->get_lengths();
        std::vector<double> angles_2 = polygon->get_angles();
        double coefficient = perimeter() / another.perimeter();
        if (!is_equal_double(
                area() / another.area(), coefficient * coefficient)) {
            return false;
        }
        for (int i = 0; i < n; ++i) {
            bool f = true;
            for (int j = 0; j < n; ++j) {
                if (!is_equal_double(
                        lengths_1[(i + j) % n] / lengths_2[j], coefficient) ||
                    !is_equal_double(angles_1[(i + j) % n], angles_2[j])) {
                    f = false;
                    break;
                }
            }
            if (f) {
                return true;
            }
        }
        return false;
    }

    bool containsPoint(Point _point) const override
    {
        Vector point = Vector(_point);
        int n = verticesCount();
        for (int i = 0; i < n; ++i) {
            if (is_equal_double(
                    (vertices[(i + 1) % n] - point) * (vertices[i] - point),
                    0)) {
                if ((vertices[(i + 1) % n] - point) % (vertices[i] - point) <
                    0) {
                    return true;
                }
            }
        }
        double sum = 0;
        for (int i = 0; i < n; ++i) {
            double sinx =
                (vertices[(i + 1) % n] - point) * (vertices[i] - point);
            double cosx =
                (vertices[(i + 1) % n] - point) % (vertices[i] - point);
            double angle = std::atan2(sinx, cosx);
            sum += angle;
        }
        return !is_equal_double(0, sum);
    }
};

class Rectangle : public Polygon
{

  public:
    ~Rectangle()
    {
    }

    Rectangle()
    {
    }

    using Polygon::Polygon;

    Rectangle(const Vector& A, const Vector& C, double tan = 1.0) : Polygon({})
    {
        vertices.resize(4);
        vertices[0] = A;
        vertices[2] = C;
        Vector Center = (A + C) / 2.0;
        tan = std::abs(tan);
        if (tan > 1.0) {
            tan = 1.0 / tan;
        }
        double angle = atan2(tan, 1.0);
        angle = -angle;
        Vector v = A - Center;
        v.rotate(2.0 * angle);
        Vector B = Center + v;
        Vector D = Center - v;
        vertices[1] = B;
        vertices[3] = D;
        make_clockwise();
    }

    Rectangle(const Point& _A, const Point& _C, double tan = 1.0) : Polygon({})
    {
        Vector A = Vector(_A);
        Vector C = Vector(_C);
        vertices.resize(4);
        vertices[0] = A;
        vertices[2] = C;
        Vector Center = (A + C) / 2.0;
        tan = std::abs(tan);
        if (tan > 1.0) {
            tan = 1.0 / tan;
        }
        double angle = atan2(tan, 1.0);
        angle = -angle;
        Vector v = A - Center;
        v.rotate(2.0 * angle);
        Vector B = Center + v;
        Vector D = Center - v;
        vertices[1] = B;
        vertices[3] = D;
        make_clockwise();
    }

    Point center() const
    {
        return Point((vertices[0] + vertices[2]) / 2.0);
    }

    std::pair<Line, Line> diagonals() const
    {
        return std::make_pair(
            Line(vertices[0], vertices[2]), Line(vertices[1], vertices[3]));
    }

    double area() const override
    {
        return dist(vertices[0], vertices[1]) * dist(vertices[0], vertices[3]);
    }

    double perimeter() const override
    {
        return (dist(vertices[0], vertices[1]) +
                dist(vertices[0], vertices[3])) *
               2.0;
    }
};

class Square : public Rectangle
{

  public:
    ~Square()
    {
    }

    Square()
    {
    }

    using Rectangle::Rectangle;

    Circle circumscribedCircle() const
    {
        return Circle(center(), dist(vertices[0], vertices[2]) / 2.0);
    }

    Circle inscribedCircle() const
    {
        return Circle(center(), dist(vertices[0], vertices[1]) / 2.0);
    }

    double area() const override
    {
        return dist(vertices[0], vertices[1]) * dist(vertices[0], vertices[1]);
    }

    double perimeter() const override
    {
        return dist(vertices[0], vertices[1]) * 4.0;
    }
};

class Triangle : public Polygon
{

  public:
    ~Triangle()
    {
    }

    Triangle()
    {
    }

    Triangle(const Vector& A, const Vector& B, const Vector& C)
        : Polygon({Point(A), Point(B), Point(C)})
    {
        make_clockwise();
    }

    Triangle(const Point& A, const Point& B, const Point& C)
        : Polygon({A, B, C})
    {
        make_clockwise();
    }

    using Polygon::Polygon;

    Circle circumscribedCircle() const
    {
        Vector first = (vertices[1] + vertices[0]) / 2.0;
        Vector first_normal = (vertices[1] - vertices[0]).rotate(pi / 2.0);
        Vector second = (vertices[2] + vertices[0]) / 2.0;
        Vector second_normal = (vertices[2] - vertices[0]).rotate(-pi / 2.0);
        Vector center = Line(first, first + first_normal)
                            .intersection(Line(second, second + second_normal));
        double radius = dist(Vector(center), Vector(vertices[0]));
        return Circle(center, radius);
    }

    Circle inscribedCircle() const
    {
        Vector a = vertices[1] - vertices[0];
        Vector b = vertices[2] - vertices[0];
        Vector bisector_0 = a * b.length() + b * a.length();
        Line line_0(vertices[0], vertices[0] + bisector_0);
        Vector bisector_1 = (vertices[0] - vertices[1]) *
                                ((vertices[2] - vertices[1]).length()) +
                            (vertices[2] - vertices[1]) *
                                ((vertices[0] - vertices[1]).length());
        Line line_1(vertices[1], vertices[1] + bisector_1);
        Vector center = line_0.intersection(line_1);
        Vector projection = Line(vertices[0], vertices[1]).projection(center);
        double radius = (center - projection).length();
        return Circle(center, radius);
    }

    Point centroid() const
    {
        return Point((vertices[0] + vertices[1] + vertices[2]) / 3.0);
    }

    Point orthocenter() const
    {
        Line a = Line(vertices[1], vertices[2]);
        Vector a_projection = a.projection(vertices[0]);
        Line b = Line(vertices[0], vertices[2]);
        Vector b_projection = b.projection(vertices[1]);
        Line first = Line(a_projection, vertices[0]);
        Line second = Line(b_projection, vertices[1]);
        return Point(first.intersection(second));
    }

    Line EulerLine() const
    {
        Line euler_line = Line(Vector(orthocenter()), Vector(centroid()));
        return euler_line;
    }

    Circle ninePointsCircle() const
    {
        Triangle inner_triangle = Triangle(
            (vertices[0] + vertices[1]) / 2.0,
            (vertices[1] + vertices[2]) / 2.0,
            (vertices[0] + vertices[2]) / 2.0);
        Circle nine_points_center = inner_triangle.circumscribedCircle();
        return nine_points_center;
    }
};