package org.im97mori.ble.android.peripheral.utils;

import android.os.Build;
import androidx.lifecycle.Lifecycle;
import androidx.test.core.app.ActivityScenario;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;
import org.im97mori.ble.android.peripheral.test.HiltTestActivity;
import org.im97mori.ble.android.peripheral.ui.BaseViewModel;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

import static junit.framework.TestCase.assertEquals;
import static org.junit.Assert.assertTrue;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class AutoDisposeViewModelProviderTest {

    public static class FakeViewModel1 extends BaseViewModel {

        public Consumer<Boolean> mDisposeConsumer;

        @Override
        public void dispose() {
            if (mDisposeConsumer != null) {
                mDisposeConsumer.accept(true);
            }
            super.dispose();
        }
    }

    @Rule
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    private ActivityScenario<HiltTestActivity> mScenario;

    @Before
    public void setUp() {
        mScenario = ActivityScenario.launch(HiltTestActivity.class);
    }

    @After
    public void tearDown() {
        mScenario.close();
    }

    @Test
    public void test_get_00001() {
        mScenario.onActivity(activity -> {
            AutoDisposeViewModelProvider autoDisposeViewModelProvider = new AutoDisposeViewModelProvider(activity, activity.getDefaultViewModelProviderFactory());
            FakeViewModel1 fakeViewModel1 = autoDisposeViewModelProvider.get(FakeViewModel1.class);
            assertEquals(FakeViewModel1.class, fakeViewModel1.getClass());
        });
    }

    @Test
    public void test_autoDispose_00001() {
        AtomicBoolean result = new AtomicBoolean(false);
        mScenario.onActivity(activity -> {
            AutoDisposeViewModelProvider autoDisposeViewModelProvider = new AutoDisposeViewModelProvider(activity, activity.getDefaultViewModelProviderFactory());
            FakeViewModel1 fakeViewModel1 = autoDisposeViewModelProvider.get(FakeViewModel1.class);
            fakeViewModel1.mDisposeConsumer = result::set;
        });
        mScenario.moveToState(Lifecycle.State.DESTROYED);

        assertTrue(result.get());
    }
}