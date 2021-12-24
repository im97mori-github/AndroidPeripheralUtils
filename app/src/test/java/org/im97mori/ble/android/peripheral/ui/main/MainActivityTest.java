package org.im97mori.ble.android.peripheral.ui.main;

import static androidx.test.espresso.Espresso.onView;
import static androidx.test.espresso.assertion.ViewAssertions.matches;
import static androidx.test.espresso.matcher.ViewMatchers.withEffectiveVisibility;
import static androidx.test.espresso.matcher.ViewMatchers.withId;
import static org.mockito.Mockito.mockStatic;

import android.os.Build;

import androidx.lifecycle.ViewModelProvider;
import androidx.test.core.app.ActivityScenario;
import androidx.test.espresso.matcher.ViewMatchers;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.utils.MockableViewModelProvider;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.MockedStatic;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.Collections;

import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class, sdk = Build.VERSION_CODES.LOLLIPOP)
public class MainActivityTest {

    @Rule
    public HiltAndroidRule hiltRule = new HiltAndroidRule(this);

    public ActivityScenario<MainActivity> scenario;

    FakeMainViewModel fakeMainViewModel;

    @Before
    public void setUp() {
        try (MockedStatic<MockableViewModelProvider> mockedStatic = mockStatic(MockableViewModelProvider.class)) {
            mockedStatic.when(() -> MockableViewModelProvider.getViewModelClass(MainViewModel.class)).thenReturn(FakeMainViewModel.class);

            scenario = ActivityScenario.launch(MainActivity.class);
            scenario.onActivity(activity -> fakeMainViewModel = new ViewModelProvider(activity).get(FakeMainViewModel.class));
        }
    }

    @After
    public void tearDown() {
        scenario.close();
    }

    @Test
    public void test_provideDeviceTypeImageResMap_00001() {
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        fakeMainViewModel.pp.onNext(Collections.emptyList());
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

}