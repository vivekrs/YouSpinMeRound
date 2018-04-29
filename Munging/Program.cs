using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Text;
using CsvHelper;
using GeoJSON.Net.Feature;
using GeoJSON.Net.Geometry;
using Newtonsoft.Json;

namespace Munging
{
    internal class Program
    {
        private static void Main(string[] args)
        {
            new TornadoDataMunging().Munge(args[0], args[1], args[2], args[3]);
        }
    }

    public class TornadoDataMunging
    {
        private readonly List<TornadoData> _result = new List<TornadoData>();

        public void Munge(string inputFilename, string allDataFilename, string countyDataFilename, string geoJsonDirectory)
        {
            using (TextReader inputFile = new StreamReader(inputFilename, Encoding.UTF8))
            {
                var csv = new CsvReader(inputFile);
                csv.Configuration.RegisterClassMap<TornadoDataReader>();
                var records = csv.GetRecords<TornadoData>();
                var instances = records
                    .Where(r => TornadoData.states.Contains(r.st))
                    .GroupBy(r => new { r.yr, r.om });//.OrderBy(g=>g.Key);

                foreach (var instance in instances)
                {
                    var key = instance.Key.yr + ":" + instance.Key.om;
                    if (instance.Count() == 1)
                    {
                        ProcessSingleInstance(instance.First());
                    }
                    else
                    {
                        var ns = instance.Select(r => r.ns).Distinct().ToList();
                        if (ns.Count != 1)
                            throw new DataException("Multiple values for ns: " + ns[0] + " for " + key);

                        switch (ns[0])
                        {
                            case 1:
                                ProcessMultiInstance(instance.ToList(), 1);
                                break;
                            case 2:
                            case 3:
                                ProcessMultiInstance(instance.ToList(), 2);
                                break;
                            default:
                                throw new DataException("Invalid value for ns: " + ns[0] + " for " + key);
                        }
                    }
                }
            }

            using (TextWriter outputFile = new StreamWriter(allDataFilename, false, Encoding.UTF8))
            {
                var csv = new CsvWriter(outputFile);
                csv.Configuration.RegisterClassMap<TornadoDataWriter>();
                csv.WriteRecords(_result);
            }

            using (TextWriter outputFile = new StreamWriter(countyDataFilename, false, Encoding.UTF8))
            {
                var data = from tornado in _result
                           from county in tornado.fipsCodes
                           let geoId = $"0500000US{tornado.stf:D2}{county:D3}"
                           select new
                           {
                               geoId,
                               tornado.inj,
                               tornado.fat,
                               tornado.dollarloss
                           };
                var groupings = from t in data
                                group t by t.geoId
                    into grp
                                select new
                                {
                                    geoId = grp.Key,
                                    inj = grp.Sum(t => t.inj),
                                    fat = grp.Sum(t => t.fat),
                                    dollarloss = grp.Sum(t => t.dollarloss)
                                };

                new CsvWriter(outputFile).WriteRecords(groupings);
            }

            foreach (var stf in _result.Select(r => r.stf).Distinct().ToList())
            {
                var features = _result.Where(t=>t.stf == stf)
                    .Select(f => new Feature(
                        new LineString(new[] { new Position(f.slat, f.slon), new Position(f.elat, f.elon) }),
                        new Dictionary<string, object> { { "tornadoId", f.id } }));
                var featureCollection = new FeatureCollection(new List<Feature>(features));
                using (TextWriter outputFile = new StreamWriter($"{geoJsonDirectory}\\{stf}.geojson", false, Encoding.UTF8))
                {
                    var jsonData = JsonConvert.SerializeObject(featureCollection);
                    outputFile.Write(jsonData);
                }
            }
        }

        private void ProcessSingleInstance(TornadoData curr)
        {
            curr.fipsCodes = curr.GetFipsCodes().ToList();
            curr.UpdateData();
            _result.Add(curr);
        }

        private void ProcessMultiInstance(IReadOnlyCollection<TornadoData> instance, int segment)
        {
            var states = instance.Where(r => r.sg == segment).ToList();
            var stateCount = new Dictionary<string, int>();
            foreach (var state in states)
            {
                if (stateCount.ContainsKey(state.st))
                    stateCount[state.st]++;
                else
                    stateCount[state.st] = 1;

                state.ctr = stateCount[state.st];
                GetCounties(state, instance);
                _result.Add(state);
            }
        }

        private static void GetCounties(TornadoData curr, IEnumerable<TornadoData> instance)
        {
            curr.fipsCodes = curr.GetFipsCodes().ToList();

            var next = instance.Where(n => n.sg == -9 && n.st == curr.st).ToList();
            foreach (var data in next)
                curr.fipsCodes.AddRange(data.GetFipsCodes());

            curr.UpdateData();
        }
    }
}