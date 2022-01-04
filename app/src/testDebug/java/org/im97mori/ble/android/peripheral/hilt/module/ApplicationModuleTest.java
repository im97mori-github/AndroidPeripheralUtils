package org.im97mori.ble.android.peripheral.hilt.module;

import static junit.framework.TestCase.assertEquals;

import android.os.Build;

import com.google.gson.FieldNamingPolicy;
import com.google.gson.Gson;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import javax.inject.Inject;

import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class ApplicationModuleTest {

    @Rule
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Inject
    Gson mGson;

    @Before
    public void setUp() {
        mHiltRule.inject();
    }

    @Test
    public void test_inject_00001() {
        assertEquals(FieldNamingPolicy.IDENTITY, mGson.fieldNamingStrategy());
    }
}