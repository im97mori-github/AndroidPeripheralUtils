package org.im97mori.ble.android.peripheral.hilt.module;

import static com.google.gson.FieldNamingPolicy.UPPER_CAMEL_CASE;
import static junit.framework.TestCase.assertEquals;

import android.os.Build;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import javax.inject.Inject;
import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;
import dagger.hilt.android.testing.UninstallModules;
import dagger.hilt.components.SingletonComponent;

@UninstallModules(ApplicationModule.class)
@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class ApplicationModuleWithUninstallTest {

    @Module
    @InstallIn(SingletonComponent.class)
    public static class FakeApplicationModule {

        @Provides
        @Singleton
        public Gson provieGson() {
            return new GsonBuilder()
                    .setFieldNamingStrategy(UPPER_CAMEL_CASE)
                    .create();
        }

    }

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
        assertEquals(UPPER_CAMEL_CASE, mGson.fieldNamingStrategy());
    }
}