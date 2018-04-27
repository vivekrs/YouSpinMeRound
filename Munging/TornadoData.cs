// ReSharper disable InconsistentNaming

using System;
using System.Collections.Generic;
using System.Linq;
using CsvHelper.Configuration;

namespace Munging
{
    public class TornadoData
    {
        private const double Tolerance = 0.001;
        private static readonly Dictionary<int, string> magnitudes = new Dictionary<int, string>
        {
            {-9, "Unknown"},
            {0, "0 - Light"},
            {1, "1 - Moderate"},
            {2, "2 - Significant"},
            {3, "3 - Severe"},
            {4, "4 - Devastating"},
            {5, "5 - Incredible"}
        };
        public static HashSet<string> states = new HashSet<string>
        {
            "AL",
            "AK",
            "AZ",
            "AR",
            "CA",
            "CO",
            "CT",
            "DE",
            "DC",
            "FL",
            "GA",
            "HI",
            "ID",
            "IL",
            "IN",
            "IA",
            "KS",
            "KY",
            "LA",
            "ME",
            "MD",
            "MA",
            "MI",
            "MN",
            "MS",
            "MO",
            "MT",
            "NE",
            "NV",
            "NH",
            "NJ",
            "NM",
            "NY",
            "NC",
            "ND",
            "OH",
            "OK",
            "OR",
            "PA",
            "RI",
            "SC",
            "SD",
            "TN",
            "TX",
            "UT",
            "VT",
            "VA",
            "WA",
            "WV",
            "WI",
            "WY"
        };

        public string id { get; set; }
        public int ctr { get; set; }
        public string om { get; set; }
        public int yr { get; set; }
        public int mo { get; set; }
        public int dy { get; set; }
        public int hr { get; set; }
        public string date { get; set; }
        public string time { get; set; }
        public string tz { get; set; }
        public string st { get; set; }
        public string stf { get; set; }
        public int stn { get; set; }
        public string mag { get; set; }
        public int inj { get; set; }
        public int fat { get; set; }
        public string loss { get; set; }
        public string dollarloss { get; set; }
        public string closs { get; set; }
        public double slat { get; set; }
        public double slon { get; set; }
        public double elat { get; set; }
        public double elon { get; set; }
        public string len { get; set; }
        public string lenkm { get; set; }
        public string wid { get; set; }
        public string widm { get; set; }
        public string chidist { get; set; }
        public string chidistkm { get; set; }
        public int ns { get; set; }
        public int sn { get; set; }
        public int sg { get; set; }
        public int f1 { get; set; }
        public int f2 { get; set; }
        public int f3 { get; set; }
        public int f4 { get; set; }
        public List<int> fipsCodes { get; set; }
        public string fips { get; set; }
        public int fc { get; set; }

        public IEnumerable<int> GetFipsCodes()
        {
            return new[] {f1, f2, f3, f4}.Where(f => f != 0);
        }

        public void UpdateData()
        {
            id = yr + "-" + om + "-" + st + "-" + ctr;
            hr = int.Parse(time.Split(':')[0]);
            if (int.TryParse(mag, out var m))
                mag = magnitudes[m];
            dollarloss = loss == "0" ? "NA" :
                yr < 1996 ? "5" + new string('0', int.Parse(loss)) :
                yr < 2016 ? $"{decimal.Parse(loss) * 1000000}" :
                loss;
            fips = string.Join(';', fipsCodes.Distinct());

            var chiDist = GetDistanceFromLatLonInKm(slat, slon, 41.881832, -87.623177);
            chidistkm = string.Format("{0:0.00}", chiDist);
            chidist = string.Format("{0:0.00}", chiDist * 0.6213712);

            widm = string.Format("{0:0.00}", double.Parse(wid) * 0.9144);
            lenkm = string.Format("{0:0.00}", double.Parse(len) * 1.609344);

            if (Math.Abs(elat) < Tolerance && Math.Abs(elon) < Tolerance)
            {
                elat = slat;
                elon = slon;
            }
        }

        private double GetDistanceFromLatLonInKm(double lat1, double lon1, double lat2, double lon2)
        {
            var R = 6371; // Radius of the earth in km
            var dLat = Deg2Rad(lat2 - lat1); // deg2rad below
            var dLon = Deg2Rad(lon2 - lon1);
            var a = Math.Sin(dLat / 2) * Math.Sin(dLat / 2) +
                    Math.Cos(Deg2Rad(lat1)) * Math.Cos(Deg2Rad(lat2)) *
                    Math.Sin(dLon / 2) * Math.Sin(dLon / 2);
            var c = 2 * Math.Atan2(Math.Sqrt(a), Math.Sqrt(1 - a));
            var d = R * c; // Distance in km
            return d;
        }

        private double Deg2Rad(double deg)
        {
            return deg * (Math.PI / 180);
        }
    }

    public sealed class TornadoDataReader : CsvClassMap<TornadoData>
    {
        public TornadoDataReader()
        {
            AutoMap();
            Map(m => m.id).Ignore();
            Map(m => m.ctr).Ignore();
            Map(m => m.hr).Ignore();
            Map(m => m.fips).Ignore();
            Map(m => m.dollarloss).Ignore();
            Map(m => m.chidistkm).Ignore();
            Map(m => m.chidist).Ignore();
            Map(m => m.widm).Ignore();
            Map(m => m.lenkm).Ignore();
        }
    }

    public sealed class TornadoDataWriter : CsvClassMap<TornadoData>
    {
        public TornadoDataWriter()
        {
            AutoMap();
            Map(m => m.ctr).Ignore();
            Map(m => m.om).Ignore();
            Map(m => m.date).Ignore();
            Map(m => m.time).Ignore();
            Map(m => m.tz).Ignore();
            Map(m => m.stn).Ignore();
            Map(m => m.loss).Ignore();
            Map(m => m.closs).Ignore();
            Map(m => m.f1).Ignore();
            Map(m => m.f2).Ignore();
            Map(m => m.f3).Ignore();
            Map(m => m.f4).Ignore();
            Map(m => m.fc).Ignore();
            Map(m => m.sn).Ignore();
            Map(m => m.sg).Ignore();
        }
    }
}