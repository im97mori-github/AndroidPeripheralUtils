package org.im97mori.ble.android.peripheral.utils;

import static junit.framework.TestCase.assertEquals;
import static org.mockito.Mockito.mockStatic;

import android.os.Build;

import androidx.lifecycle.ViewModel;
import androidx.lifecycle.ViewModelStore;

import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.MockedStatic;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

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
public class MockableViewModelProviderTest {

    public static class FakeViewModel1 extends ViewModel {
    }

    public static class FakeViewModel2 extends FakeViewModel1 {
    }

    @Rule
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Test
    public void test_get_00001() {
        MockableViewModelProvider mockableViewModelProvider = new MockableViewModelProvider(ViewModelStore::new);
        FakeViewModel1 fakeViewModel1 = mockableViewModelProvider.get(FakeViewModel1.class);
        assertEquals(FakeViewModel1.class, fakeViewModel1.getClass());
    }

    @Test
    public void test_get_00002() {
        try (MockedStatic<MockableViewModelProvider> mockedStatic = mockStatic(MockableViewModelProvider.class)) {
            mockedStatic.when(()
                    -> MockableViewModelProvider.getViewModelClass(FakeViewModel1.class)).thenReturn(FakeViewModel2.class);


            MockableViewModelProvider mockableViewModelProvider = new MockableViewModelProvider(ViewModelStore::new);
            FakeViewModel1 fakeViewModel1 = mockableViewModelProvider.get(FakeViewModel1.class);
            assertEquals(FakeViewModel2.class, fakeViewModel1.getClass());
        }
    }

    @Test
    public void test_getViewModelClass_00001() {
        Class<? extends FakeViewModel1> clazz = MockableViewModelProvider.getViewModelClass(FakeViewModel1.class);
        assertEquals(FakeViewModel1.class, clazz);
    }

    @Test
    public void test_getViewModelClass_00002() {
        try (MockedStatic<MockableViewModelProvider> mockedStatic = mockStatic(MockableViewModelProvider.class)) {
            mockedStatic.when(()
                    -> MockableViewModelProvider.getViewModelClass(FakeViewModel1.class)).thenReturn(FakeViewModel2.class);


            Class<? extends FakeViewModel1> clazz = MockableViewModelProvider.getViewModelClass(FakeViewModel1.class);
            assertEquals(FakeViewModel2.class, clazz);
        }
    }

}