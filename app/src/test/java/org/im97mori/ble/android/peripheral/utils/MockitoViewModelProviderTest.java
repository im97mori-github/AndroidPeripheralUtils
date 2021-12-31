package org.im97mori.ble.android.peripheral.utils;

import static junit.framework.TestCase.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mockStatic;

import android.os.Build;

import androidx.lifecycle.Lifecycle;
import androidx.test.core.app.ActivityScenario;

import org.im97mori.ble.android.peripheral.ui.BaseViewModel;
import org.im97mori.ble.android.peripheral.ui.main.MainActivity;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.MockedStatic;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

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
public class MockitoViewModelProviderTest {

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

    public static class FakeViewModel2 extends FakeViewModel1 {
    }

    @Rule
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    private ActivityScenario<MainActivity> mScenario;

    @Before
    public void setUp() {
        mScenario = ActivityScenario.launch(MainActivity.class);
    }

    @After
    public void tearDown() {
        mScenario.close();
    }

    @Test
    public void test_get_00001() {
        mScenario.onActivity(activity -> {
            MockitoViewModelProvider mockitoViewModelProvider = new MockitoViewModelProvider(activity);
            FakeViewModel1 fakeViewModel1 = mockitoViewModelProvider.get(FakeViewModel1.class);
            assertEquals(FakeViewModel1.class, fakeViewModel1.getClass());
        });
    }

    @Test
    public void test_get_00002() {
        try (MockedStatic<MockitoViewModelProvider> mockedStatic = mockStatic(MockitoViewModelProvider.class)) {
            mockedStatic.when(()
                    -> MockitoViewModelProvider.getViewModelClass(FakeViewModel1.class)).thenReturn(FakeViewModel2.class);

            mScenario.onActivity(activity -> {
                MockitoViewModelProvider mockitoViewModelProvider = new MockitoViewModelProvider(activity);
                FakeViewModel1 fakeViewModel1 = mockitoViewModelProvider.get(FakeViewModel1.class);
                assertEquals(FakeViewModel2.class, fakeViewModel1.getClass());
            });
        }
    }

    @Test
    public void test_getViewModelClass_00001() {
        Class<? extends FakeViewModel1> clazz = MockitoViewModelProvider.getViewModelClass(FakeViewModel1.class);
        assertEquals(FakeViewModel1.class, clazz);
    }

    @Test
    public void test_getViewModelClass_00002() {
        try (MockedStatic<MockitoViewModelProvider> mockedStatic = mockStatic(MockitoViewModelProvider.class)) {
            mockedStatic.when(()
                    -> MockitoViewModelProvider.getViewModelClass(FakeViewModel1.class)).thenReturn(FakeViewModel2.class);


            Class<? extends FakeViewModel1> clazz = MockitoViewModelProvider.getViewModelClass(FakeViewModel1.class);
            assertEquals(FakeViewModel2.class, clazz);
        }
    }

    @Test
    public void test_autoDispose_00001() {
        AtomicBoolean result = new AtomicBoolean(false);
        mScenario.onActivity(activity -> {
            MockitoViewModelProvider mockitoViewModelProvider = new MockitoViewModelProvider(activity);
            FakeViewModel1 fakeViewModel1 = mockitoViewModelProvider.get(FakeViewModel1.class);
            fakeViewModel1.mDisposeConsumer = result::set;
        });
        mScenario.moveToState(Lifecycle.State.DESTROYED);

        assertTrue(result.get());
    }
}