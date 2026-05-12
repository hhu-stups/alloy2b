package de.hhu.stups.alloy2b;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.stream.Collectors;

import de.hhu.stups.alloy2b.translation.Alloy2BParser;
import de.hhu.stups.alloy2b.translation.Alloy2BParserErr;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class Alloy2BParserTests {
    private static String getResourceAsString(String resourceName) throws IOException {
        InputStream expected = Alloy2BParserTests.class.getResourceAsStream("/" + resourceName);
        if (expected != null) {
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(expected, StandardCharsets.UTF_8))) {
                return reader.lines().collect(Collectors.joining("\n")).trim();
            }
        }
        throw new FileNotFoundException();
    }

    @Test
    void translateFileSystem() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("filesystem.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/FileSystem.als").getPrologTerm());
    }

    @Test
    void translateFileSystem2() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("filesystem2.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/FileSystem2.als").getPrologTerm());
    }

    @Test
    void translateCards() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("cards.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/cards.als").getPrologTerm());
    }

    @Test
    void translateSelfGrandpas() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("selfgrandpas.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/SelfGrandpas.als").getPrologTerm());
    }

    @Test
    void translateCrewAlloc() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("crewalloc.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/crewalloc.als").getPrologTerm());
    }

    @Test
    void translateQueens() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("queens.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/queens.als").getPrologTerm());
    }

    @Test
    void translateQueens2() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("queens2.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/queens2.als").getPrologTerm());
    }

    @Test
    void translateRiverCrossing() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("rivercrossingpuzzle.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/RiverCrossingPuzzle.als").getPrologTerm());
    }

    @Test
    void translateHanoi() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("hanoi.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/hanoi.als").getPrologTerm());
    }

    @Test
    void translateEnumTest() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("enumtest.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/EnumTest.als").getPrologTerm());
    }

    @Test
    void translateGraphiso() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("graphiso.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/graphiso.als").getPrologTerm());
    }

    @Test
    void translateDisjFieldTest() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("disjfieldtest.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/DisjFieldTest.als").getPrologTerm());
    }

    @Test
    void translateFamilyV1() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("family-v1.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/family-v1.als").getPrologTerm());
    }

    @Test
    void translateRelations() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("relations.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/relations.als").getPrologTerm());
    }

    @Test
    void translateViews() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("views.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/views.als").getPrologTerm());
    }

    @Test
    void translateSequenceTests() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("sequence_test.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/sequence_test.als").getPrologTerm());
    }

    @Test
    void translateMutex() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("mutex.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/mutex.als").getPrologTerm());
    }

    @Test
    void translateHttp() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("http.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/http.als").getPrologTerm());
    }

    @Test
    void translateKnightsAndKnaves() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("knights_and_knaves.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/knights_and_knaves.als").getPrologTerm());
    }

    @Test
    void translateJoinBinary() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("joinbinary.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/JoinBinary.als").getPrologTerm());
    }

    @Test
    void translateJoinBinaryComplex() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("joinbinarycomplex.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/JoinBinaryComplex.als").getPrologTerm());
    }

    @Test
    void translateArithmetic() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("arithmetic.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/Arithmetic.als").getPrologTerm());
    }

    @Test
    void translateRelLaw() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("laws/rellaws.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/laws/RelLaws.als").getPrologTerm());
    }

    @Test
    void translateAllocLarge() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("alloc_large.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/alloc_large.als").getPrologTerm());
    }

    @Test
    void translateOrderingTest() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("ordering_test.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/ordering_test.als").getPrologTerm());
    }

    @Test
    void translateGenerated200() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("generated200.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/generated200.als").getPrologTerm());
    }

    @Test
    void translateSlotData() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("slot_data_v2.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/slot_data_v2.als").getPrologTerm());
    }

    @Test
    void translateJobPuzzle() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("job_puzzle.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/job_puzzle.als").getPrologTerm());
    }

    /*@Test
    void translateGenerated400() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("generated400.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/generated400.als").getPrologTerm());
    }*/

    @Test
    void translateNamespaces() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("namespace_module_a.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/namespace_module_a.als").getPrologTerm());
    }

    @Test
    void translateNamespaces2() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("moduleb.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/ModuleB.als").getPrologTerm());
    }

    @Test
    void translateUtilInteger() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("util_integer.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/util_integer.als").getPrologTerm());
    }

    @Test
    void translateQuantifiers() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("quantifiers.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/quantifiers.als").getPrologTerm());
    }

    @Test
    void translateSeqPreconditions() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("seq_preconditions.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/seq_preconditions.als").getPrologTerm());
    }

    @Test
    void translateNaturals() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("natural.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/natural.als").getPrologTerm());
    }

    @Test
    void translateBinaryDifferentlyTyped() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("binary_differently_typed.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/binary_differently_typed.als").getPrologTerm());
    }

    @Test
    void translateLists() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("lists.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/lists.als").getPrologTerm());
    }

    @Test
    void translateRestrictions() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("restrictions.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/Restrictions.als").getPrologTerm());
    }

    @Test
    void translateUtilBool() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("utilbool.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/utilbool.als").getPrologTerm());
    }

    @Test
    void translateAmbiguousFieldName() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("ambiguous_field_name.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/ambiguous_field_name.als").getPrologTerm());
    }

    @Test
    void translateQuantifierAmbiguousFieldName() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("ambiguous_quantifier_field_name.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/ambiguous_quantifier_field_name.als").getPrologTerm());
    }

    @Test
    void translateEinstein1() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("einstein1.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/einstein1.als").getPrologTerm());
    }

    @Test
    void translateEinstein2() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("einstein2.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/einstein2.als").getPrologTerm());
    }

    @Test
    void translateMultiplicities() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("multiplicities.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/multiplicities.als").getPrologTerm());
    }

    @Test
    void translateIdenIn() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("iden_in.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/iden_in.als").getPrologTerm());
    }

    @Test
    void translateSudoku() throws Alloy2BParserErr, IOException {
        String expected = getResourceAsString("sudoku1.pl");
        assertEquals(expected, new Alloy2BParser().parseFromFile("/sudoku1.als").getPrologTerm());
    }
}
