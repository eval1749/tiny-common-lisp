// setbuild - set build numbers
//  setbuild.js
//
//  This file is part of Project Vogue.
//
//  Copyright (C) 1996-2007 by Project Vogue. All rights reserved.
//  Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
//  @(#)$Id: //proj/evedit2/mainline/setbuild.cs#1 $
//
//
// Format of build.txt
//  1 major.minor.build[.revision]
//  2 product name
//  3 trademark
//  4 Company
//  5 copyright text
//  6 last updated datetime
//  7 YYYY-MM (start year and month)
//  8 ... comments ...
//
using System;
using System.Collections;
using System.IO;
using System.Text;

class Application
{

class MyException : Exception
{
    public MyException(string strFormat, params object[] rgoArg) :
        base(String.Format(strFormat, rgoArg))
    {
        // nothing to do
    } 
} // MyException


//////////////////////////////////////////////////////////////////////
//
// Info class
//
class Info
{
    public uint    Major;
    public uint    Minor;
    public uint    Build;
    public uint    Revision;

    public string Company   { get { return (string) Map["Company"]; } }
    public string Copyright { get { return (string) Map["Copyright"]; } }
    public string Product   { get { return (string) Map["Product"]; } }

    public Hashtable Map = new Hashtable();

    public static Info Parse(string strLine)
    {
        Info oInfo = new Info();

        string[] rgstrField = strLine.Split('.');

        switch (rgstrField.Length)
        {
        case 3:
            oInfo.Revision = 1;
            break;

        case 4:
            oInfo.Revision = UInt32.Parse(rgstrField[3]);
            break;

        default:
            throw new MyException("Expect m.n.b[.i]: {0}", strLine);
        } // cFields

        oInfo.Major = UInt32.Parse(rgstrField[0]);
        oInfo.Minor = UInt32.Parse(rgstrField[1]);
        oInfo.Build = UInt32.Parse(rgstrField[2]);
        return oInfo;
    } // Parse

    public override string ToString()
    {
        if (1 == Revision)
        {
            return String.Format("{0}.{1}.{2:D4}", Major, Minor, Build);
        }
        else
        {
            return String.Format(
                "{0}.{1}.{2:D4}.{3}",
                Major, Minor, Build, Revision );
        }
    } // ToString
} // Info

static Encoding s_oEncoding = Encoding.GetEncoding(1252);

//////////////////////////////////////////////////////////////////////
//
// Process build.h
//
static void
process_build_h(Info oInfo)
{
    if (! File.Exists("build.h"))
    {
        return;
    }

    using (FileStream oFile = new FileStream("build.h", FileMode.Create))
    using (StreamWriter oWriter = new StreamWriter(oFile, s_oEncoding))
    {
        oWriter.WriteLine("#if defined(UNICODE)");
        oWriter.WriteLine("#define MY_Product \"{0}\"", oInfo.Product);
        oWriter.WriteLine("#define MY_ProductW L\"{0}\"", oInfo.Product);
        oWriter.WriteLine("#else // defined(UNICODE)");
        oWriter.WriteLine("#define MY_Product \"{0}\"", oInfo.Product);
        oWriter.WriteLine("#define MY_ProductW L\"{0}\"", oInfo.Product);
        oWriter.WriteLine("#endif // defined(UNICODE)");

        oWriter.WriteLine("#define MY_VersionMajor {0}", oInfo.Major);
        oWriter.WriteLine("#define MY_VersionMinor {0}", oInfo.Minor);
        oWriter.WriteLine("#define MY_VersionBuild {0}", oInfo.Build);
        oWriter.WriteLine("#define MY_VersionIncre {0}", oInfo.Revision);

        oWriter.WriteLine("#define MY_FILEVERSION {0},{1},{2},{3}",
                         oInfo.Major,
                         oInfo.Minor,
                         oInfo.Build,
                         oInfo.Revision );

        oWriter.WriteLine("#define MY_FileVersion \"{0}.{1}.{2:D4}.{3}\\0\"",
                         oInfo.Major,
                         oInfo.Minor,
                         oInfo.Build,
                         oInfo.Revision );

        oWriter.WriteLine("#define MY_FileVersionW L\"{0}.{1}.{2:D4}.{3}\\0\"",
                         oInfo.Major,
                         oInfo.Minor,
                         oInfo.Build,
                         oInfo.Revision );

        oWriter.WriteLine("#define MY_CompanyName \"{0}\\0\"",
            oInfo.Company );

        oWriter.WriteLine("#define MY_LegalCopyright   \"{0}\\0\"",
            oInfo.Copyright );

        oWriter.Close();
    } // using
} // process_build_h


//////////////////////////////////////////////////////////////////////
//
// Process version.cs
//
static void
process_version_cs(string strDir, Info oInfo)
{
    foreach (string strPath in Directory.GetFiles(strDir))
    {
        if (0 == String.Compare(Path.GetFileName(strPath), "version.cs"))
        {
            update_version_cs(strPath, oInfo);
        }
    } // for each file

    foreach (string strPath in Directory.GetDirectories(strDir))
    {
        process_version_cs(strPath, oInfo);
    } // for each file
} // process_version_cs


//////////////////////////////////////////////////////////////////////
//
// Update build.txt
//
static Info
update_build_txt()
{
    Info oInfo;
    uint    nYear;
    uint    nMonth;
    StringBuilder oRest = new StringBuilder();

    using (FileStream oFile = new FileStream("build.txt", FileMode.Open))
    using (StreamReader oReader = new StreamReader(oFile, s_oEncoding))
    {
        oInfo = Info.Parse(oReader.ReadLine());
        oInfo.Map["Product"]   = oReader.ReadLine().Trim();
        oInfo.Map["Trademark"] = oReader.ReadLine().Trim();
        oInfo.Map["Company"]   = oReader.ReadLine().Trim();
        oInfo.Map["Copyright"] = oReader.ReadLine().Trim();

        oReader.ReadLine(); // date

        uint nYM = parse_YYYY_MM(oReader.ReadLine());
            nYear  = nYM / 100;
            nMonth = nYM % 100;

        for (;;)
        {
            string strLine = oReader.ReadLine();
            if (null == strLine)
            {
                break;
            }
            oRest.Append(strLine);
            oRest.Append("\n");
        } // for each line
    } // using oReader

    DateTime dteNow = DateTime.Now;

    uint nDD = (uint) dteNow.Day;
    uint nMM = (uint) (dteNow.Year - nYear) * 12;
         nMM += (uint) dteNow.Month + 1;
         nMM -= nMonth;

    if (nMM * 100 + nDD == oInfo.Build)
    {
        oInfo.Revision += 1;
    }
    else
    {
        oInfo.Build = nMM * 100 + nDD;
        oInfo.Revision = 1;
    }

    using (FileStream oFile = new FileStream("#build.txt", FileMode.Create))
    using (StreamWriter oWriter = new StreamWriter(oFile, s_oEncoding))
    {
        oWriter.WriteLine(oInfo);
        oWriter.WriteLine(oInfo.Product);
        oWriter.WriteLine(oInfo.Map["Trademark"]);
        oWriter.WriteLine(oInfo.Company);
        oWriter.WriteLine(oInfo.Copyright);

        oWriter.WriteLine(
            "{0:yyyy}-{0:MM}-{0:dd} {0:HH}:{0:mm}:{0:ss}",
            dteNow );

        oWriter.WriteLine("{0}-{1:D2}", nYear, nMonth);
        oWriter.Write(oRest.ToString());
    } // using oWriter

    File.Delete("build.txt");
    File.Move("#build.txt", "build.txt");

    oInfo.Map["Version"] = oInfo.ToString();

    return oInfo;
} // update_build_txt


//////////////////////////////////////////////////////////////////////
//
// Update "version.cs"
//
static void
update_version_cs(string strPath, Info oInfo)
{
    Console.WriteLine(strPath);

    string strTemp = Path.Combine(
        Path.GetDirectoryName(strPath),
        "#" + Path.GetFileName(strPath) );

    using (StreamReader oReader = new StreamReader(strPath))
    using (StreamWriter oWriter = new StreamWriter(strTemp))
    {
        for (;;)
        {
            string strLine = oReader.ReadLine();
            if (null == strLine)
            {
                break;
            }

            foreach (string strKey in oInfo.Map.Keys)
            {
                if (strLine.StartsWith("[assembly: Assembly" + strKey + "("))
                {
                    strLine = String.Format(
                        "[assembly: Assembly{0}(\"{1}\")]",
                        strKey,
                        oInfo.Map[strKey] );
                    break;
                }
            } // for each key

            oWriter.WriteLine(strLine);
        } // for each line
    } // using

    File.Delete(strPath);
    File.Move(strTemp, strPath);
} // update_version_cs


//////////////////////////////////////////////////////////////////////
//
// Parser YYYY-MM
//
static uint parse_YYYY_MM(string strLine)
{
    string[] rgstrField = strLine.Split('-');
    if (2 != rgstrField.Length)
    {
        throw new MyException("Expect YYYY-MM: {0}", strLine);
    }

    uint nY = UInt32.Parse(rgstrField[0]);
    if (nY < 1979 || nY > DateTime.Now.Year)
    {
        throw new MyException("Invalid year: {0}", nY);
    }

    uint nM = UInt32.Parse(rgstrField[1]);

    if (nM < 1 || nM > 12)
    {
        throw new MyException("Expect [1,12]: {0}", nM);
    }

    return nY * 100 + nM;
} // parse_YYYY_MM


//////////////////////////////////////////////////////////////////////
//
// Entry Point
//
[STAThread]
public static void
Main()
{
        Info oInfo = update_build_txt();
        process_build_h(oInfo);
        process_version_cs(".", oInfo);
        Console.WriteLine(oInfo);
} // Main

} // Application
