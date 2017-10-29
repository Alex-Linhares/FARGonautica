using System;
using System.Collections.Generic;
using System.Text;

namespace RhythmCat
{
    public class CodeletAttribute : Attribute
    {

        public enum CodeletWorkType
        {
            Create, Destroy, Examine, Halt
        }


        private const int DEFAULT_URGENCY = 10;
        private string _category;
        private CodeletWorkType _workType;
        private int _defaultUrgency;
        private bool _active;

        public bool Active
        {
            get { return _active; }
            set { _active = value; }
        }

        public string Category
        {
            get { return _category; }
        }

        public int DefaultUrgency
        {
            get { return _defaultUrgency; }
        }


        public CodeletAttribute(string category, CodeletWorkType workType, int defaultUrgency, bool active)
        {
            _category = category;
            _workType = workType;
            _defaultUrgency = defaultUrgency;
            _active = active;
        }

        public CodeletAttribute(string category, CodeletWorkType workType, bool active)
        {
            _category = category;
            _workType = workType;
            _defaultUrgency = DEFAULT_URGENCY;
            _active = active;
        }
    }

    public abstract class OptimizableParameterAttribute : Attribute
    {

        public enum ParameterDataType
        {
            DoubleParameter, IntParameter, BoolParameter
        }

        protected ParameterDataType _parameterDataType;

        public ParameterDataType ParameterType
        {
            get { return _parameterDataType; }
            set { _parameterDataType = value; }
        }


    }


    public class TopDownCodeletAttribute : Attribute
    {

    }

    public class OptimizableBoolParameterAttribute : OptimizableParameterAttribute
    {
        public OptimizableBoolParameterAttribute()
        {
            _parameterDataType = ParameterDataType.BoolParameter;
        }
    }


    public class OptimizableDoubleParameterAttribute : OptimizableParameterAttribute
    {
        private double _rangeMin;

        public double RangeMin
        {
            get { return _rangeMin; }
            set { _rangeMin = value; }
        }

        private double _rangeMax;

        public double RangeMax
        {
            get { return _rangeMax; }
            set { _rangeMax = value; }
        }

        public OptimizableDoubleParameterAttribute(double min, double max)
        {
            _rangeMin = min;
            _rangeMax = max;

            _parameterDataType = ParameterDataType.DoubleParameter;

        }
    }

    public class OptimizableIntParameterAttribute : OptimizableParameterAttribute
    {
        private int _rangeMin;

        public int RangeMin
        {
            get { return _rangeMin; }
            set { _rangeMin = value; }
        }

        private int _rangeMax;

        public int RangeMax
        {
            get { return _rangeMax; }
            set { _rangeMax = value; }
        }

        public OptimizableIntParameterAttribute(int min, int max)
        {
            _rangeMin = min;
            _rangeMax = max;

            _parameterDataType = ParameterDataType.IntParameter;
        }
    }
}
