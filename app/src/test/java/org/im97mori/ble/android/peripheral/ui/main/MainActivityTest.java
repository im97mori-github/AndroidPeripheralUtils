package org.im97mori.ble.android.peripheral.ui.main;

import static androidx.test.espresso.Espresso.onData;
import static androidx.test.espresso.Espresso.onView;
import static androidx.test.espresso.action.ViewActions.click;
import static androidx.test.espresso.assertion.ViewAssertions.matches;
import static androidx.test.espresso.intent.Intents.intended;
import static androidx.test.espresso.intent.matcher.IntentMatchers.hasComponent;
import static androidx.test.espresso.intent.matcher.IntentMatchers.hasExtra;
import static androidx.test.espresso.matcher.ViewMatchers.withEffectiveVisibility;
import static androidx.test.espresso.matcher.ViewMatchers.withId;
import static androidx.test.espresso.matcher.ViewMatchers.withParent;
import static androidx.test.espresso.matcher.ViewMatchers.withText;
import static junit.framework.TestCase.assertTrue;
import static org.hamcrest.Matchers.is;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_ID;
import static org.mockito.Mockito.mockStatic;

import android.content.ComponentName;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.drawable.Drawable;
import android.os.Build;
import android.view.View;
import android.widget.TextView;

import androidx.lifecycle.ViewModelProvider;
import androidx.test.core.app.ActivityScenario;
import androidx.test.core.app.ApplicationProvider;
import androidx.test.espresso.intent.Intents;
import androidx.test.espresso.matcher.BoundedMatcher;
import androidx.test.espresso.matcher.ViewMatchers;

import com.google.android.gms.oss.licenses.OssLicensesMenuActivity;
import com.google.android.material.appbar.MaterialToolbar;

import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;
import org.im97mori.ble.android.peripheral.Constants;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.room.Device;
import org.im97mori.ble.android.peripheral.test.TestUtils;
import org.im97mori.ble.android.peripheral.ui.device.PeripheralActivity;
import org.im97mori.ble.android.peripheral.ui.device.type.DeviceTypeListActivity;
import org.im97mori.ble.android.peripheral.utils.MockableViewModelProvider;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.MockedStatic;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.Arrays;
import java.util.Collections;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;

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

    private ActivityScenario<MainActivity> scenario;

    private FakeMainViewModel fakeMainViewModel;

    @Before
    public void setUp() {
        hiltRule.inject();
//        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
//        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());
        try (MockedStatic<MockableViewModelProvider> mockedStatic = mockStatic(MockableViewModelProvider.class)) {
            mockedStatic.when(() -> MockableViewModelProvider.getViewModelClass(MainViewModel.class)).thenReturn(FakeMainViewModel.class);

            scenario = ActivityScenario.launch(MainActivity.class);
            scenario.onActivity(activity -> fakeMainViewModel = new ViewModelProvider(activity).get(FakeMainViewModel.class));
        }
        Intents.init();
    }

    @After
    public void tearDown() {
        Intents.release();
        scenario.close();
    }

    @Test
    public void test_title_00001() {
        fakeMainViewModel.getDeviceListProcessor.onNext(Collections.emptyList());
        onView(withText(R.string.app_name)).check(matches(withParent(withId(R.id.topAppBar))));
    }

    @Test
    public void test_root_container_visibility_00001() {
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        fakeMainViewModel.getDeviceListProcessor.onNext(Collections.emptyList());
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_grid_visibility_00001() {
        onView(withId(R.id.grid)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        fakeMainViewModel.getDeviceListProcessor.onNext(Collections.emptyList());
        onView(withId(R.id.grid)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_grid_visibility_00002() {
        onView(withId(R.id.grid)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        fakeMainViewModel.getDeviceListProcessor.onNext(Collections.singletonList(new Device(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null)));
        onView(withId(R.id.grid)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_grid_click_00001() {
        final Device device = new Device(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        fakeMainViewModel.getDeviceListProcessor.onNext(Collections.singletonList(device));
        onData(is(new BoundedMatcher<Object, Device>(Device.class) {
            @Override
            public void describeTo(Description description) {
                description.appendText("Device: " + device.hashCode());
            }

            @Override
            protected boolean matchesSafely(Device target) {
                return device == target;
            }
        }))
                .perform(click());
        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), PeripheralActivity.class)));
        intended(hasExtra(KEY_DEVICE_ID, device.getId()));
    }

    @Test
    public void test_grid_click_00002() {
        final Device device1 = new Device(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        final Device device2 = new Device(2, "b", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        fakeMainViewModel.getDeviceListProcessor.onNext(Arrays.asList(device1, device2));
        onData(is(new BoundedMatcher<Object, Device>(Device.class) {
            @Override
            public void describeTo(Description description) {
                description.appendText("Device: " + device2.hashCode());
            }

            @Override
            protected boolean matchesSafely(Device target) {
                return device2 == target;
            }
        }))
                .perform(click());
        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), PeripheralActivity.class)));
        intended(hasExtra(KEY_DEVICE_ID, device2.getId()));
    }

    @Test
    public void test_grid_text_00001() {
        final Device device = new Device(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        fakeMainViewModel.getDeviceListProcessor.onNext(Collections.singletonList(device));
        onData(is(new BoundedMatcher<Object, Device>(Device.class) {
            @Override
            public void describeTo(Description description) {
                description.appendText("Device: " + device.hashCode());
            }

            @Override
            protected boolean matchesSafely(Device target) {
                return device == target;
            }
        })).check(matches(withText(device.getDeviceSettingName())));
    }

    @Test
    public void test_grid_text_00002() {
        final Device device1 = new Device(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        final Device device2 = new Device(2, "b", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        fakeMainViewModel.getDeviceListProcessor.onNext(Arrays.asList(device1, device2));
        onData(is(new BoundedMatcher<Object, Device>(Device.class) {
            @Override
            public void describeTo(Description description) {
                description.appendText("Device: " + device2.hashCode());
            }

            @Override
            protected boolean matchesSafely(Device target) {
                return device2 == target;
            }
        })).check(matches(withText(device2.getDeviceSettingName())));
    }

    @Test
    public void test_grid_image_00001() {
        final Device device = new Device(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        fakeMainViewModel.getDeviceListProcessor.onNext(Collections.singletonList(device));
        onData(is(new BoundedMatcher<Object, Device>(Device.class) {
            @Override
            public void describeTo(Description description) {
                description.appendText("Device: " + device.hashCode());
            }

            @Override
            protected boolean matchesSafely(Device target) {
                return device == target;
            }
        })).check(matches(new TypeSafeMatcher<View>() {
            @Override
            protected boolean matchesSafely(View item) {
                TextView textView = item.findViewById(R.id.grid_text);
                Bitmap targetBitmap = TestUtils.getBitmap(textView.getCompoundDrawablesRelative()[1]);
                Bitmap bitmap = TestUtils.getBitmap(item.getContext().getDrawable(Objects.requireNonNull(fakeMainViewModel.provideDeviceTypeImageResMap().get(device.getDeviceType()))));
                return targetBitmap.sameAs(bitmap);
            }

            @Override
            public void describeTo(Description description) {
                description.appendText("device_type:" + device.getDeviceType());
            }
        }));
    }

    @Test
    public void test_grid_image_00002() {
        final Device device1 = new Device(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        final Device device2 = new Device(2, "b", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        fakeMainViewModel.getDeviceListProcessor.onNext(Arrays.asList(device1, device2));
        onData(is(new BoundedMatcher<Object, Device>(Device.class) {
            @Override
            public void describeTo(Description description) {
                description.appendText("Device: " + device2.hashCode());
            }

            @Override
            protected boolean matchesSafely(Device target) {
                return device2 == target;
            }
        })).check(matches(new TypeSafeMatcher<View>() {
            @Override
            protected boolean matchesSafely(View item) {
                TextView textView = item.findViewById(R.id.grid_text);
                Bitmap targetBitmap = TestUtils.getBitmap(textView.getCompoundDrawablesRelative()[1]);
                Bitmap bitmap = TestUtils.getBitmap(item.getContext().getDrawable(Objects.requireNonNull(fakeMainViewModel.provideDeviceTypeImageResMap().get(device2.getDeviceType()))));
                return targetBitmap.sameAs(bitmap);
            }

            @Override
            public void describeTo(Description description) {
                description.appendText("device_type:" + device2.getDeviceType());
            }
        }));
    }

    @Test
    public void test_emptyView_visibility_00001() {
        onView(withId(R.id.empty)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        fakeMainViewModel.getDeviceListProcessor.onNext(Collections.emptyList());
        onView(withId(R.id.empty)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_emptyView_visibility_00002() {
        onView(withId(R.id.empty)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        fakeMainViewModel.getDeviceListProcessor.onNext(Collections.singletonList(new Device(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null)));
        onView(withId(R.id.empty)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_menu_create_device_00001() {
        scenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withText(R.string.menu_create_device)).perform(click());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), DeviceTypeListActivity.class)));
    }

    @Test
    public void test_menu_clear_devices_00001() {
        final AtomicBoolean result = new AtomicBoolean(false);
        fakeMainViewModel.setDeleteAllDevicesConsumer(result::set);

        scenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withText(R.string.menu_clear_devices)).perform(click());

        assertTrue(result.get());
    }

    @Test
    public void test_menu_license_00001() {
        scenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withText(R.string.menu_license)).perform(click());
        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), OssLicensesMenuActivity.class)));
    }

}