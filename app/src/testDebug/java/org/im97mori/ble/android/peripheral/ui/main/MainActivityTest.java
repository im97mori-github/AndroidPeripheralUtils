package org.im97mori.ble.android.peripheral.ui.main;

import android.app.Activity;
import android.app.Instrumentation.ActivityResult;
import android.content.ComponentName;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.drawable.Drawable;
import android.os.Build;
import android.view.View;
import android.widget.TextView;
import androidx.lifecycle.HasDefaultViewModelProviderFactory;
import androidx.lifecycle.ViewModelProvider;
import androidx.test.core.app.ActivityScenario;
import androidx.test.core.app.ApplicationProvider;
import androidx.test.espresso.intent.Intents;
import androidx.test.espresso.matcher.BoundedMatcher;
import androidx.test.espresso.matcher.ViewMatchers;
import com.google.android.gms.oss.licenses.OssLicensesMenuActivity;
import com.google.android.material.appbar.MaterialToolbar;
import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;
import dagger.hilt.android.testing.UninstallModules;
import dagger.hilt.components.SingletonComponent;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;
import org.im97mori.ble.android.peripheral.Constants;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.hilt.module.ViewModelFactoryFunctionModule;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;
import org.im97mori.ble.android.peripheral.test.FakeViewModelProviderFactoryFunction;
import org.im97mori.ble.android.peripheral.test.TestUtils;
import org.im97mori.ble.android.peripheral.ui.device.PeripheralActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.DeviceSettingActivity;
import org.im97mori.ble.android.peripheral.ui.device.type.DeviceTypeListActivity;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import javax.inject.Singleton;
import java.util.Arrays;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;

import static androidx.test.espresso.Espresso.onData;
import static androidx.test.espresso.Espresso.onView;
import static androidx.test.espresso.action.ViewActions.click;
import static androidx.test.espresso.assertion.ViewAssertions.matches;
import static androidx.test.espresso.intent.Intents.*;
import static androidx.test.espresso.intent.matcher.IntentMatchers.hasComponent;
import static androidx.test.espresso.intent.matcher.IntentMatchers.hasExtra;
import static androidx.test.espresso.matcher.ViewMatchers.*;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.assertTrue;
import static org.hamcrest.Matchers.is;
import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_UNDEFINED;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.*;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
@UninstallModules(ViewModelFactoryFunctionModule.class)
public class MainActivityTest {

    @Module
    @InstallIn(SingletonComponent.class)
    interface FakeViewModelFactoryFunctionModule {
        @Singleton
        @Provides
        public static Function<HasDefaultViewModelProviderFactory, ViewModelProvider.Factory> bindViewModelProviderFactoryFunction() {
            FakeViewModelProviderFactoryFunction fakeViewModelProviderFactoryFunction = new FakeViewModelProviderFactoryFunction();
            fakeViewModelProviderFactoryFunction.setFakeViewModelClass(MainViewModel.class, FakeMainViewModel.class);
            return fakeViewModelProviderFactoryFunction;
        }
    }

    @Rule
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    private ActivityScenario<MainActivity> mScenario;

    private FakeMainViewModel mViewModel;

    @Before
    public void setUp() {
        mHiltRule.inject();
        mScenario = ActivityScenario.launch(MainActivity.class);
        mScenario.onActivity(activity -> mViewModel = (FakeMainViewModel) new ViewModelProvider(activity).get(MainViewModel.class));
        Intents.init();
    }

    @After
    public void tearDown() {
        Intents.release();
        mScenario.close();
    }

    @Test
    public void test_title_00001() {
        onView(withId(R.id.topAppBar)).check(matches(hasDescendant(withText(R.string.app_name))));
    }

    @Test
    public void test_root_container_visibility_00001() {
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveAllDeviceSettingSubject.onNext(Collections.emptyList());
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_grid_visibility_00001() {
        onView(withId(R.id.grid)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveAllDeviceSettingSubject.onNext(Collections.emptyList());
        onView(withId(R.id.grid)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_grid_visibility_00002() {
        onView(withId(R.id.grid)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveAllDeviceSettingSubject.onNext(Collections.singletonList(new DeviceSetting(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null)));
        onView(withId(R.id.grid)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_grid_click_00001() {
        final DeviceSetting deviceSetting = new DeviceSetting(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        mViewModel.mObserveAllDeviceSettingSubject.onNext(Collections.singletonList(deviceSetting));
        onData(is(new BoundedMatcher<Object, DeviceSetting>(DeviceSetting.class) {
            @Override
            public void describeTo(Description description) {
                description.appendText("Device: " + deviceSetting.hashCode());
            }

            @Override
            protected boolean matchesSafely(DeviceSetting target) {
                return deviceSetting == target;
            }
        }))
                .perform(click());
        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), PeripheralActivity.class)));
        intended(hasExtra(KEY_DEVICE_ID, deviceSetting.getId()));
    }

    @Test
    public void test_grid_click_00002() {
        final DeviceSetting deviceSetting1 = new DeviceSetting(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        final DeviceSetting deviceSetting2 = new DeviceSetting(2, "b", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        mViewModel.mObserveAllDeviceSettingSubject.onNext(Arrays.asList(deviceSetting1, deviceSetting2));
        onData(is(new BoundedMatcher<Object, DeviceSetting>(DeviceSetting.class) {
            @Override
            public void describeTo(Description description) {
                description.appendText("Device: " + deviceSetting2.hashCode());
            }

            @Override
            protected boolean matchesSafely(DeviceSetting target) {
                return deviceSetting2 == target;
            }
        }))
                .perform(click());
        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), PeripheralActivity.class)));
        intended(hasExtra(KEY_DEVICE_ID, deviceSetting2.getId()));
    }

    @Test
    public void test_grid_text_00001() {
        final DeviceSetting deviceSetting = new DeviceSetting(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        mViewModel.mObserveAllDeviceSettingSubject.onNext(Collections.singletonList(deviceSetting));
        onData(is(new BoundedMatcher<Object, DeviceSetting>(DeviceSetting.class) {
            @Override
            public void describeTo(Description description) {
                description.appendText("Device: " + deviceSetting.hashCode());
            }

            @Override
            protected boolean matchesSafely(DeviceSetting target) {
                return deviceSetting == target;
            }
        })).check(matches(withText(deviceSetting.getDeviceSettingName())));
    }

    @Test
    public void test_grid_text_00002() {
        final DeviceSetting deviceSetting1 = new DeviceSetting(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        final DeviceSetting deviceSetting2 = new DeviceSetting(2, "b", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        mViewModel.mObserveAllDeviceSettingSubject.onNext(Arrays.asList(deviceSetting1, deviceSetting2));
        onData(is(new BoundedMatcher<Object, DeviceSetting>(DeviceSetting.class) {
            @Override
            public void describeTo(Description description) {
                description.appendText("Device: " + deviceSetting2.hashCode());
            }

            @Override
            protected boolean matchesSafely(DeviceSetting target) {
                return deviceSetting2 == target;
            }
        })).check(matches(withText(deviceSetting2.getDeviceSettingName())));
    }

    @Test
    public void test_grid_image_00001() {
        final DeviceSetting deviceSetting = new DeviceSetting(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        mViewModel.mObserveAllDeviceSettingSubject.onNext(Collections.singletonList(deviceSetting));
        onData(is(new BoundedMatcher<Object, DeviceSetting>(DeviceSetting.class) {
            @Override
            public void describeTo(Description description) {
                description.appendText("Device: " + deviceSetting.hashCode());
            }

            @Override
            protected boolean matchesSafely(DeviceSetting target) {
                return deviceSetting == target;
            }
        })).check(matches(new TypeSafeMatcher<View>() {
            @Override
            protected boolean matchesSafely(View item) {
                TextView textView = item.findViewById(R.id.grid_text);
                Bitmap targetBitmap = TestUtils.getBitmap(textView.getCompoundDrawablesRelative()[1]);
                Integer resId = mViewModel.provideDeviceTypeImageResMap().get(deviceSetting.getDeviceType());
                assertNotNull(resId);
                Drawable drawable = item.getContext().getDrawable(resId);
                assertNotNull(drawable);
                Bitmap bitmap = TestUtils.getBitmap(drawable);
                return targetBitmap.sameAs(bitmap);
            }

            @Override
            public void describeTo(Description description) {
                description.appendText("device_type:" + deviceSetting.getDeviceType());
            }
        }));
    }

    @Test
    public void test_grid_image_00002() {
        final DeviceSetting deviceSetting1 = new DeviceSetting(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        final DeviceSetting deviceSetting2 = new DeviceSetting(2, "b", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        mViewModel.mObserveAllDeviceSettingSubject.onNext(Arrays.asList(deviceSetting1, deviceSetting2));
        onData(is(new BoundedMatcher<Object, DeviceSetting>(DeviceSetting.class) {
            @Override
            public void describeTo(Description description) {
                description.appendText("Device: " + deviceSetting2.hashCode());
            }

            @Override
            protected boolean matchesSafely(DeviceSetting target) {
                return deviceSetting2 == target;
            }
        })).check(matches(new TypeSafeMatcher<View>() {
            @Override
            protected boolean matchesSafely(View item) {
                TextView textView = item.findViewById(R.id.grid_text);
                Bitmap targetBitmap = TestUtils.getBitmap(textView.getCompoundDrawablesRelative()[1]);
                Integer resId = mViewModel.provideDeviceTypeImageResMap().get(deviceSetting2.getDeviceType());
                assertNotNull(resId);
                Drawable drawable = item.getContext().getDrawable(resId);
                assertNotNull(drawable);
                Bitmap bitmap = TestUtils.getBitmap(drawable);
                return targetBitmap.sameAs(bitmap);
            }

            @Override
            public void describeTo(Description description) {
                description.appendText("device_type:" + deviceSetting2.getDeviceType());
            }
        }));
    }

    @Test
    public void test_emptyView_visibility_00001() {
        onView(withId(R.id.empty)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveAllDeviceSettingSubject.onNext(Collections.emptyList());
        onView(withId(R.id.empty)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_emptyView_visibility_00002() {
        onView(withId(R.id.empty)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveAllDeviceSettingSubject.onNext(Collections.singletonList(new DeviceSetting(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null)));
        onView(withId(R.id.empty)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_menu_00001() {
        mViewModel.mObserveAllDeviceSettingSubject.onNext(Collections.singletonList(new DeviceSetting(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null)));
        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withText(R.string.menu_create_device)).perform(click());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), DeviceTypeListActivity.class)));
    }

    @Test
    public void test_menu_create_device_00001() {
        mViewModel.mObserveAllDeviceSettingSubject.onNext(Collections.singletonList(new DeviceSetting(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null)));
        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withText(R.string.menu_create_device)).perform(click());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), DeviceTypeListActivity.class)));
    }

    @Test
    public void test_menu_clear_devices_00001() {
        final AtomicBoolean result = new AtomicBoolean(false);
        mViewModel.mObserveDeleteAllDeviceSettingAction = () -> result.set(true);
        mViewModel.mObserveAllDeviceSettingSubject.onNext(Collections.singletonList(new DeviceSetting(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null)));
        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withText(R.string.menu_clear_devices)).perform(click());

        assertTrue(result.get());
    }

    @Test
    public void test_menu_license_00001() {
        mViewModel.mObserveAllDeviceSettingSubject.onNext(Collections.singletonList(new DeviceSetting(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null)));
        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withText(R.string.menu_license)).perform(click());
        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), OssLicensesMenuActivity.class)));
    }

    @Test
    public void test_activity_result_00001() {
        Intent resultData = new Intent();
        resultData.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_UNDEFINED);
        ActivityResult result = new ActivityResult(Activity.RESULT_OK, resultData);
        intending(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), DeviceTypeListActivity.class))).respondWith(result);

        mViewModel.mObserveAllDeviceSettingSubject.onNext(Collections.singletonList(new DeviceSetting(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null)));
        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withText(R.string.menu_create_device)).perform(click());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), DeviceSettingActivity.class)));
        intended(hasExtra(KEY_DEVICE_ID, VALUE_DEVICE_ID_UNSAVED));
        intended(hasExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_UNDEFINED));
    }

    @Test
    public void test_activity_result_00002() {
        ActivityResult result = new ActivityResult(Activity.RESULT_CANCELED, null);
        intending(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), DeviceTypeListActivity.class))).respondWith(result);

        mViewModel.mObserveAllDeviceSettingSubject.onNext(Collections.singletonList(new DeviceSetting(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null)));
        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withText(R.string.menu_create_device)).perform(click());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), DeviceTypeListActivity.class)));
        assertNoUnverifiedIntents();
    }

    @Test
    public void test_recreate_00001() {
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveAllDeviceSettingSubject.onNext(Collections.emptyList());
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveAllDeviceSettingSubject.onNext(Collections.emptyList());
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

    }

    @Test
    public void test_recreate_00002() {
        onView(withId(R.id.grid)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveAllDeviceSettingSubject.onNext(Collections.singletonList(new DeviceSetting(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null)));
        onView(withId(R.id.grid)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.grid)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveAllDeviceSettingSubject.onNext(Collections.singletonList(new DeviceSetting(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null)));
        onView(withId(R.id.grid)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

}