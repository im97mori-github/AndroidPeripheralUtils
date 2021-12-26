package org.im97mori.ble.android.peripheral.utils;

import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertTrue;

import android.os.Build;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;

import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class MapObserverTest {

    @Test
    public void test_onChanged_00001() {
        final AtomicBoolean result = new AtomicBoolean(false);
        MapObserver<String, Boolean> mapObserver = new MapObserver<>(Objects::isNull, result::set);
        mapObserver.onChanged(null);
        assertTrue(result.get());
    }

    @Test
    public void test_onChanged_00002() {
        final AtomicBoolean result = new AtomicBoolean(true);
        MapObserver<String, Boolean> mapObserver = new MapObserver<>(Objects::isNull, result::set);
        mapObserver.onChanged("");
        assertFalse(result.get());
    }

}