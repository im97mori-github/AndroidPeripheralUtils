package org.im97mori.ble.android.peripheral.ui.main;

import android.os.Build;

import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(application = HiltTestApplication.class, sdk = Build.VERSION_CODES.LOLLIPOP)
public class MainViewModelTest {

    @Rule
    public HiltAndroidRule hiltRule = new HiltAndroidRule(this);

    @Test
    public void test_provideDeviceTypeImageResMap_00001() {
//        assert false;
    }

}