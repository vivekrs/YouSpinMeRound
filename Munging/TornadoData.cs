// ReSharper disable InconsistentNaming

using System;
using System.Collections.Generic;
using System.Linq;
using CsvHelper.Configuration;

namespace Munging
{
    public class TornadoData
    {
        public string id { get; set; }
        public string om { get; set; }
        public int yr { get; set; }
        public int mo { get; set; }
        public int dy { get; set; }
        public string date { get; set; }
        public string time { get; set; }
        public string tz { get; set; }
        public string st { get; set; }
        public string stf { get; set; }
        public int stn { get; set; }
        public int mag { get; set; }
        public int inj { get; set; }
        public int fat { get; set; }
        public string loss { get; set; }
        public string dollarloss { get; set; }
        public string closs { get; set; }
        public string slat { get; set; }
        public string slon { get; set; }
        public string elat { get; set; }
        public string elon { get; set; }
        public string len { get; set; }
        public string wid { get; set; }
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
            return new[] { f1, f2, f3, f4 }.Where(f => f != 0);
        }

        public void UpdateData()
        {
            id = yr + "-" + om + "-" + st;
            dollarloss = loss == "0" ? "NA" :
                yr < 1996 ? "5" + new string('0', int.Parse(loss)) :
                yr < 2016 ? $"{decimal.Parse(loss) * 1000000}" :
                loss;
            fips = string.Join(';', fipsCodes.Distinct());
        }
    }

    public sealed class TornadoDataIgnoreFips : CsvClassMap<TornadoData>
    {
        public TornadoDataIgnoreFips()
        {
            AutoMap();
            Map(m => m.id).Ignore();
            Map(m => m.fips).Ignore();
            Map(m => m.dollarloss).Ignore();
        }
    }

    public sealed class TornadoDataIgnoreFns : CsvClassMap<TornadoData>
    {
        public TornadoDataIgnoreFns()
        {
            AutoMap();
            Map(m => m.om).Ignore();
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