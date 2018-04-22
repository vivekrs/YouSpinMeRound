using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Text;
using CsvHelper;

namespace Munging
{
    internal class Program
    {
        private static void Main(string[] args)
        {
            new TornadoDataMunging().Munge(args[0], args[1]);
        }
    }

    public class TornadoDataMunging
    {
        private readonly List<TornadoData> _result = new List<TornadoData>();

        public void Munge(string inputFilename, string outputFilename)
        {
            using (TextReader inputFile = new StreamReader(inputFilename, Encoding.UTF8))
            {
                var csv = new CsvReader(inputFile);
                csv.Configuration.RegisterClassMap<TornadoDataReader>();
                var records = csv.GetRecords<TornadoData>();
                var instances = records
                    .Where(r=>TornadoData.states.Contains(r.st))
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

            using (TextWriter outputFile = new StreamWriter(outputFilename, false, Encoding.UTF8))
            {
                var csv = new CsvWriter(outputFile);
                csv.Configuration.RegisterClassMap<TornadoDataWriter>();
                csv.WriteRecords(_result);
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