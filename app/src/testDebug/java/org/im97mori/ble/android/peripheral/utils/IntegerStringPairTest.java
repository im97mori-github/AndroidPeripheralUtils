package org.im97mori.ble.android.peripheral.utils;

import static junit.framework.TestCase.assertEquals;

import android.os.Build;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class IntegerStringPairTest {

    @Test
    public void test_toString_00001() {
        String second = "2";
        IntegerStringPair pair = new IntegerStringPair(1, second);
        assertEquals(second, pair.toString());
    }

}