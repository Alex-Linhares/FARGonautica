using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Reflection;
using RhythmCat;

namespace PrintAllCodelets {
    public partial class Form1 : Form {
        public Form1() {
            InitializeComponent();

            Alphabet al = new Alphabet();
            Type alphabetType = al.GetType();

            StringBuilder sbEnabled = new StringBuilder();
            StringBuilder sbDisabled = new StringBuilder();
            StringBuilder sbGraph = new StringBuilder();

            sbGraph.AppendLine("digraph codelets {");

            int active = 0;
            int disabled = 0;

            sbEnabled.AppendLine("Active Codelets:");
            sbEnabled.AppendLine("----------------");

            sbDisabled.AppendLine("Disabled Codelets:");
            sbDisabled.AppendLine("----------------");



            Assembly asm = Assembly.GetAssembly(alphabetType);
            foreach (Type t in asm.GetExportedTypes()) {
                foreach (Attribute a in t.GetCustomAttributes(false)) {
                    CodeletAttribute ca = a as CodeletAttribute;
                    if (ca != null) {
                        Codelet c = (Codelet)asm.CreateInstance(t.FullName, false, BindingFlags.CreateInstance, null,
                                    new object[] { ca.DefaultUrgency, null, null, null, null },
                                    null, null);
                        StringBuilder s;
                        if (ca.Active) {
                            s = sbEnabled;
                            active++;
                        } else {
                            s = sbDisabled;
                            disabled++;
                        }
                        s.AppendLine(c.Name);
                        sbGraph.AppendFormat("\t\"{0}\"->\"child\";", c.Name);
                        sbGraph.AppendLine();

                        // Find constructors.
                        int i = 1;
                        foreach (ConstructorInfo info in t.GetConstructors()) {
                            s.AppendFormat("\tConstructor {0}", i++);
                            s.AppendLine();
                            foreach (ParameterInfo pInfo in info.GetParameters()) {
                                if (pInfo.Name == "urgency" ||
                                    pInfo.Name == "parent" ||
                                    pInfo.Name == "workspace" ||
                                    pInfo.Name == "coderack" ||
                                    pInfo.Name == "slipnet")
                                    continue;
                                s.AppendFormat("\t\t{0}: {1}", pInfo.ParameterType.Name, pInfo.Name);
                                s.AppendLine();
                            }
                        }
                    }
                }
            }


            sbEnabled.AppendLine();
            sbEnabled.AppendLine("total: ");
            sbEnabled.AppendLine(active.ToString());
            
            sbDisabled.AppendLine();
            sbDisabled.AppendLine("total: ");
            sbDisabled.AppendLine(disabled.ToString());

            sbGraph.AppendLine("overlap=false");
            sbGraph.AppendLine("}");

            textBox1.Text = sbEnabled.ToString();
            textBox2.Text = sbDisabled.ToString();
            textBox3.Text = sbGraph.ToString();
        }
    }
}
